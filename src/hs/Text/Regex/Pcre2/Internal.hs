{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Pcre2.Internal where

import           Control.Applicative        (Alternative(..))
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Either                (partitionEithers)
import           Data.Foldable              (foldl', toList)
import           Data.Function              (fix)
import           Data.Functor.Identity      (Identity(..))
import           Data.IORef
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IM
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Alt(..), First)
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Foreign          as Text
import           Data.Typeable              (cast)
import           Data.Void                  (Void, absurd)
import           Foreign
import           Foreign.C.Types            (CInt(..), CUChar, CUInt(..))
import qualified Foreign.Concurrent         as Conc
import           Lens.Micro
import           Lens.Micro.Extras          (preview, view)
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Regex.Pcre2.Foreign

-- * General utilities

type FfiWrapper f = f -> IO (FunPtr f)

-- | There is no @nullForeignPtr@ to pass to `withForeignPtr`, so we have to
-- fake it with a `Maybe`.
withForeignOrNullPtr :: Maybe (ForeignPtr a) -> (Ptr a -> IO b) -> IO b
withForeignOrNullPtr = maybe ($ nullPtr) withForeignPtr

-- | Helper so we never leak untracked 'Ptr's.
mkForeignPtr :: (Ptr a -> IO ()) -> IO (Ptr a) -> IO (ForeignPtr a)
mkForeignPtr finalize create = create >>= Conc.newForeignPtr <*> finalize

-- | Helper so we never leak untracked 'FunPtr's.
mkFunPtr :: ForeignPtr b -> IO (FunPtr a) -> IO (FunPtr a)
mkFunPtr anchor create = do
    funPtr <- create
    Conc.addForeignPtrFinalizer anchor $ freeHaskellFunPtr funPtr
    return funPtr

bitOr :: (Foldable t, Bits a) => t a -> a
bitOr = foldl' (.|.) zeroBits

-- | Like `lines`, but don't remove any characters.
unchompedLines :: String -> [String]
unchompedLines s = case break (== '\n') s of
    (line,     _ : rest) -> (line ++ "\n") : unchompedLines rest
    (lastLine, "")       -> [lastLine | not $ null lastLine]

-- | Equivalent to @flip fix@.
--
-- Used to express a recursive function of one argument that is called only once
-- on an initial value:
--
-- > let go x = ... in go x0
--
-- as:
--
-- > fix1 x0 $ \go x -> ...
fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 x f = fix f x

-- | Like `fix1`, but for a function of two arguments.  Currently unused.
fix2 :: a -> b -> ((a -> b -> c) -> a -> b -> c) -> c
fix2 x y f = fix f x y

-- | Like `fix1`, but for a function of three arguments.  Currently unused.
fix3 :: a -> b -> c -> ((a -> b -> c -> d) -> a -> b -> c -> d) -> d
fix3 x y z f = fix f x y z

-- ** Fast @Text@ slicing

data Slice = Slice
    {-# UNPACK #-} !Text.I8
    {-# UNPACK #-} !Text.I8

-- | Zero-copy slice a 'Text'.  An unset capture is represented by a
-- `pcre2_UNSET` range and is interpreted in this library as `Text.empty`.
thinSlice :: Text -> Slice -> Text
thinSlice text slice = fromMaybe Text.empty $ maybeThinSlice text slice

-- | Like `thinSlice`, but encode `pcre2_UNSET` as `Nothing`.
maybeThinSlice :: Text -> Slice -> Maybe Text
maybeThinSlice text (Slice off offEnd)
    | off == fromIntegral pcre2_UNSET = Nothing
    | otherwise                       = Just $ text
        & Text.takeWord8 offEnd
        & Text.dropWord8 off

-- | Slice a 'Text', copying if it's less than half of the original.  Note this
-- is a lazy, pure operation.
smartSlice :: Text -> Slice -> Text
smartSlice text slice = fromMaybe Text.empty $ maybeSmartSlice text slice

-- | Like `smartSlice`, but only produce a result when not `pcre2_UNSET`.  It is
-- forced when the outer `Maybe` constructor is forced.
maybeSmartSlice :: Text -> Slice -> Maybe Text
maybeSmartSlice text slice = f <$!> maybeThinSlice text slice where
    f substring
        | Text.length substring > Text.length text `div` 2 = substring
        | otherwise                                        = Text.copy substring

-- | Safe, type-restricted `castPtr`.
fromCUs :: Ptr CUChar -> Ptr Word8
fromCUs = castPtr

-- | Safe, type-restricted `castPtr`.
toCUs :: Ptr Word8 -> Ptr CUChar
toCUs = castPtr

-- ** Lens utilities

-- | A more general `toListOf` that collects targets into any `Alternative`.
toAlternativeOf :: (Alternative f) => Getting (Alt f a) s a -> s -> f a
toAlternativeOf l = let alt = Alt . pure in getAlt . view (l . to alt)

_Identity :: Lens' (Identity a) a
_Identity f (Identity x) = Identity <$> f x

-- ** Streaming support

-- | A @FreeT@-style stream that can short-circuit.
data Stream b m a
    = StreamPure a
    | StreamYield b (Stream b m a)    -- ^ Yield a value and keep going.
    | StreamEffect (m (Stream b m a)) -- ^ Have an effect and keep going.
    | StreamStop                      -- ^ Short-circuit.
    deriving (Functor)

instance (Functor m) => Applicative (Stream b m) where
    pure = StreamPure
    StreamPure f     <*> sx = f <$> sx
    StreamYield y sf <*> sx = StreamYield y $ sf <*> sx
    StreamEffect msf <*> sx = StreamEffect $ msf <&> (<*> sx)
    StreamStop       <*> _  = StreamStop

instance (Functor m) => Monad (Stream b m) where
    StreamPure x     >>= f = f x
    StreamYield y sx >>= f = StreamYield y $ sx >>= f
    StreamEffect msx >>= f = StreamEffect $ msx <&> (>>= f)
    StreamStop       >>= _ = StreamStop

instance MonadTrans (Stream b) where
    lift = StreamEffect . fmap StreamPure

instance (MonadIO m) => MonadIO (Stream b m) where
    liftIO = lift . liftIO

streamYield :: b -> Stream b m ()
streamYield y = StreamYield y $ StreamPure ()

-- | Effectfully transform yielded values.
mapMS :: (Functor m) => (b -> m c) -> Stream b m a -> Stream c m a
mapMS f = fix $ \go -> \case
    StreamPure x     -> StreamPure x
    StreamYield y sx -> StreamEffect $ f y <&> \y' -> StreamYield y' $ go sx
    StreamEffect ms  -> StreamEffect $ go <$> ms
    StreamStop       -> StreamStop

-- | Unsafely, lazily tear down a `Stream` into a pure list of values yielded.
-- The stream must be infinite in the sense of it only terminating due to
-- explicit `StreamStop`.
unsafeLazyStreamToList :: Stream b IO Void -> [b]
unsafeLazyStreamToList = fix $ \continue -> \case
    StreamPure v    -> absurd v
    StreamYield y s -> y : continue s
    StreamEffect ms -> continue $ unsafePerformIO ms
    StreamStop      -> []

-- * Assembling inputs into @Matcher@s and @Subber@s

-- | A matching function where all inputs and auxilliary data structures have
-- been \"compiled\".  It takes a subject a produces a stream of match results
-- corresponding to a global match.
--
-- The actual values of type @Ptr Pcre2_match_data@ will be equal within each
-- global match; they represent the states of the C data at moments in time, and
-- are intended to be composed with another streaming transformation before
-- being subjected to teardown and `unsafePerformIO`.
type Matcher = Text -> Stream (Ptr Pcre2_match_data) IO Void

-- | A substitution function.  It takes a subject and produces the number of
-- substitutions performed (0 or 1, or more in the presence of `SubGlobal`)
-- along with the transformed subject.  Currently the number is unused.
type Subber = Text -> IO (CInt, Text)

-- ** Options

-- | A `Monoid` representing nearly every facility PCRE2 presents for tweaking
-- the behavior of regex compilation and execution.
--
-- All library functions that take options have the suffix @Opt@ in their names;
-- for each of them, there's also a non-@Opt@ convenience function that simply
-- has the (unexported) `mempty` option.  For many uses, options won't be
-- needed.
--
-- Some options can be enabled by special character sequences in the pattern as
-- an alternative to specifying them as an `Option`.  See `Caseless` for
-- example.
--
-- Most options are exported in "Text.Regex.Pcre2".  The callout interface is
-- found in "Text.Regex.Pcre2.Unsafe".
--
-- Documentation is scant here.  For more complete, accurate information,
-- including discussions of corner cases arising from specific combinations of
-- options and pattern items, please see the [C API
-- documentation](https://pcre.org/current/doc/html/pcre2api.html).
data Option
    = NoOptions -- ^ `mempty`
    | TwoOptions Option Option -- ^ `<>`

    | AllowEmptyClass -- ^ Make @[]@ not match anything, rather than counting
    -- the @]@ as the first character of the class.
    | AltBsux -- ^ Like `AltBsuxLegacy`, except with ECMAScript 6 hex literal
    -- feature for @\\u@.
    | AltBsuxLegacy -- ^ Behave like ECMAScript 5 for @\\U@, @\\u@, and @\\x@.
    -- See 'AltBsux'.
    | AltCircumflex -- ^ Match a @^@ after a newline at the end of the subject.
    -- Only relevant in multiline mode.
    | AltVerbNames -- ^ Enable backslash escapes in verb names.  E.g.,
    -- @(*MARK:L\\(O\\)L)@.
    | Anchored -- ^ Equivalent to beginning pattern with @^@.
    | BadEscapeIsLiteral -- ^ Do not throw an error for unrecognized or
    -- malformed escapes.  /"This is a dangerous option."/
    | Bsr Bsr -- ^ Override what @\\R@ matches (default given by `defaultBsr`).
    | Caseless -- ^ Case-insensitive match.  Equivalent to @(?i)@.
    | DepthLimit Word32 -- ^ Override maximum depth of nested backtracking
    -- (default given by `defaultDepthLimit`).  Equivalent to
    -- @(*LIMIT_DEPTH=@/number/@)@.
    | DollarEndOnly -- ^ Don't match @$@ with a newline at the end of the
    -- subject.
    | DotAll -- ^ A dot also matches a (single-character) newline.  Equivalent
    -- to @(?s)@.
    | EndAnchored -- ^ More or less like ending pattern with @$@.
    | EscapedCrIsLf -- ^ Interpret @\\r@ as @\\n@.
    | Extended -- ^ In the pattern, ignore whitespace, and enable comments
    -- starting with @#@.  Equivalent to @(?x)@.
    | ExtendedMore -- ^ Like `Extended` but also ignore spaces and tabs within
    -- @[]@.
    | FirstLine -- ^ The match must begin in the first line of the subject.
    | HeapLimit Word32 -- ^ Override maximum heap memory (in kibibytes) used to
    -- hold backtracking information (default given by `defaultHeapLimit`).
    -- Equivalent to @(*LIMIT_HEAP=@/number/@)@.
    | Literal -- ^ Treat the pattern as a literal string.
    | MatchLimit Word32 -- ^ Override maximum value of the main matching loop's
    -- internal counter (default given by `defaultMatchLimit`), as a simple CPU
    -- throttle.  Equivalent to @(*LIMIT_MATCH=@/number/@)@.
    | MatchLine -- ^ Only match complete lines.  Equivalent to bracketing the
    -- pattern with @^(?:@/pattern/@)$@.
    | MatchUnsetBackRef -- ^ A backreference to an unset capture group matches
    -- an empty string.
    | MatchWord -- ^ Only match subjects that have word boundaries at the
    -- beginning and end.  Equivalent to bracketing the pattern with
    -- @\\b(?:@/pattern/@)\\b@.
    | MaxPatternLength Word64 -- ^ Default is `maxBound`.
    | Multiline -- ^ @^@ and @$@ mean "beginning\/end of a line" rather than
    -- "beginning\/end of the subject".  Equivalent to @(?m)@.
    | NeverBackslashC -- ^ Do not allow the unsafe @\\C@ sequence.
    | NeverUcp -- ^ Don't count Unicode characters in some character classes
    -- such as @\\d@.  Overrides @(*UCP)@.
    | Newline Newline -- ^ Override what a newline is (default given by
    -- `defaultNewline`).  Equivalent to @(*CRLF)@ or similar.
    | NoAutoCapture -- ^ Disable numbered capturing parentheses.
    | NoAutoPossess -- ^ Turn off some optimizations, possibly resulting in some
    -- callouts not being called.
    | NoDotStarAnchor -- ^ Turn off an optimization involving @.*@, possibly
    -- resulting in some callouts not being called.
    | NoStartOptimize -- ^ Turn off some optimizations normally performed at the
    -- beginning of a pattern.
    | NotBol -- ^ First character of subject is not the __b__eginning __o__f
    -- __l__ine.  Only affects @^@.
    | NotEmpty -- ^ The 0th capture doesn't match if it would be empty.
    | NotEmptyAtStart -- ^ The 0th capture doesn't match if it would be empty
    -- and at the beginning of the subject.
    | NotEol -- ^ End of subject is not the __e__nd __o__f __l__ine.  Only
    -- affects @$@.
    | OffsetLimit Word64 -- ^ Limit how far an unanchored search can advance in
    -- the subject.
    | ParensLimit Word32 -- ^ Override max depth of nested parentheses (default
    -- given by `defaultParensLimit`).
    | PartialHard -- ^ If the subject ends without finding a complete match,
    -- stop trying alternatives and signal a partial match immediately.
    -- Currently we do this by throwing a `Pcre2Exception` but we should do
    -- better.
    | PartialSoft -- ^ If the subject ends and all alternatives have been tried,
    -- but no complete match is found, signal a partial match.  Currently we do
    -- this by throwing a `Pcre2Exception` but we should do better.
    | SubGlobal -- ^ /Affects `subOpt`./  Replace all, rather than just the
    -- first.
    | SubLiteral -- ^ /Affects `subOpt`./  Treat the replacement as a literal
    -- string.
    | SubReplacementOnly -- ^ /Affects `subOpt`./  Return just the rendered
    -- replacement instead of it within the subject.  With `SubGlobal`, all
    -- results are concatenated.
    | SubUnknownUnset -- ^ /Affects `subOpt`./  References in the replacement to
    -- non-existent captures don't error but are treated as unset.
    | SubUnsetEmpty -- ^ /Affects `subOpt`./  References in the replacement to
    -- unset captures don't error but are treated as empty.
    | Ucp -- ^ Count Unicode characters in some character classes such as @\\d@.
    -- Incompatible with `NeverUcp`.
    | Ungreedy -- ^ Invert the effect of @?@.  Without it, quantifiers are
    -- non-greedy; with it, they are greedy.  Equivalent to @(?U)@.

    | UnsafeCompileRecGuard (Int -> IO Bool) -- ^ Run the given guard on every
    -- new descent into a level of parentheses, passing the current depth as
    -- argument.  Returning @False@ aborts pattern compilation with an
    -- exception.  Multiples of this option before the rightmost are ignored.
    --
    -- /NOTE: Currently (PCRE2 version 10\.39) patterns seem to be compiled in/
    -- /two passes, both times triggering the recursion guard.  Also, it is/
    -- /triggered at the beginning of the pattern, passing 0.  None of this is/
    -- /documented; expect the unexpected in the presence of side effects!/
    | UnsafeCallout (CalloutInfo -> IO CalloutResult) -- ^ Run the given callout
    -- at every callout point (see
    -- [the docs](https://pcre.org/current/doc/html/pcre2callout.html) for more
    -- info).  Multiples of this option before the rightmost are ignored.
    | AutoCallout -- ^ Run callout for every pattern item.  Only relevant if a
    -- callout is set.
    | UnsafeSubCallout (SubCalloutInfo -> IO SubCalloutResult) -- ^ Run the
    -- given callout on every substitution.  This is at most once unless
    -- `SubGlobal` is set.  Multiples of this option before the rightmost are
    -- ignored.

instance Semigroup Option where
    (<>) = TwoOptions

instance Monoid Option where
    mempty = NoOptions

-- | What @\\R@, __b__ack__s__lash __R__, can mean.
data Bsr
    = BsrUnicode -- ^ any Unicode line ending sequence
    | BsrAnyCrlf -- ^ @\\r@, @\\n@, or @\\r\\n@
    deriving (Eq, Show)

-- | C to Haskell.
bsrFromC :: CUInt -> Bsr
bsrFromC x
    | x == pcre2_BSR_UNICODE = BsrUnicode
    | x == pcre2_BSR_ANYCRLF = BsrAnyCrlf
    | otherwise              = error $ "bsrFromC: bad value " ++ show x

-- | Haskell to C.
bsrToC :: Bsr -> CUInt
bsrToC BsrUnicode = pcre2_BSR_UNICODE
bsrToC BsrAnyCrlf = pcre2_BSR_ANYCRLF

-- | What's considered a newline.
data Newline
    = NewlineCr      -- ^ @\\r@ only
    | NewlineLf      -- ^ @\\n@ only
    | NewlineCrlf    -- ^ @\\r\\n@ only
    | NewlineAny     -- ^ any Unicode line ending sequence
    | NewlineAnyCrlf -- ^ any of the above
    | NewlineNul     -- ^ binary zero
    deriving (Eq, Show)

-- | C to Haskell.
newlineFromC :: CUInt -> Newline
newlineFromC x
    | x == pcre2_NEWLINE_CR      = NewlineCr
    | x == pcre2_NEWLINE_LF      = NewlineLf
    | x == pcre2_NEWLINE_CRLF    = NewlineCrlf
    | x == pcre2_NEWLINE_ANY     = NewlineAny
    | x == pcre2_NEWLINE_ANYCRLF = NewlineAnyCrlf
    | x == pcre2_NEWLINE_NUL     = NewlineNul
    | otherwise                  = error $ "newlineFromC: bad value " ++ show x

-- | Haskell to C.
newlineToC :: Newline -> CUInt
newlineToC NewlineCr      = pcre2_NEWLINE_CR
newlineToC NewlineLf      = pcre2_NEWLINE_LF
newlineToC NewlineCrlf    = pcre2_NEWLINE_CRLF
newlineToC NewlineAny     = pcre2_NEWLINE_ANY
newlineToC NewlineAnyCrlf = pcre2_NEWLINE_ANYCRLF
newlineToC NewlineNul     = pcre2_NEWLINE_NUL

-- | Input for user-defined callouts.
data CalloutInfo
    = CalloutInfo{
        -- | The index of which callout point we're on.
        calloutIndex :: CalloutIndex,
        -- | The captures that have been set so far.
        calloutCaptures :: NonEmpty (Maybe Text),
        -- | The original subject.
        calloutSubject :: Text,
        -- | The name of the most recently passed @(*MARK)@, @(*PRUNE)@, or
        -- @(*THEN)@, if any.
        calloutMark :: Maybe Text,
        -- | Is this the first callout after the start of matching?
        calloutIsFirst :: Bool,
        -- | Has a backtrack occurred since the previous callout, or the
        -- beginning of matching if no previous callouts?
        calloutBacktracked :: Bool}
    deriving (Show, Eq)

-- | What caused the callout.
data CalloutIndex
    = CalloutNumber Int -- ^ Numerical callout.
    | CalloutName Text -- ^ String callout.
    | CalloutAuto Int Int -- ^ The item located at this half-open range of
    -- offsets within the pattern.  See `AutoCallout`.
    deriving (Show, Eq)

-- | Callout functions return one of these values, which dictates what happens
-- next in the match.
data CalloutResult
    = CalloutProceed -- ^ Keep going.
    | CalloutNoMatchHere -- ^ Fail the current capture, but not the whole match.
    -- For example, backtracking may occur.
    | CalloutNoMatch -- ^ Fail the whole match.
    deriving (Show, Eq)

-- | Input for user-defined substitution callouts.
data SubCalloutInfo
    = SubCalloutInfo{
        -- | The 1-based index of which substitution we're on.  Only goes past 1
        -- during global substitutions.
        subCalloutSubsCount :: Int,
        -- | The captures that have been set so far.
        subCalloutCaptures :: NonEmpty (Maybe Text),
        -- | The original subject.
        subCalloutSubject :: Text,
        -- | The replacement.
        subCalloutReplacement :: Text}
    deriving (Show, Eq)

-- | Substitution callout functions return one of these values, which dictates
-- what happens next in the substitution.
data SubCalloutResult
    = SubCalloutAccept -- ^ Succeed, and keep going if in global mode.
    | SubCalloutSkip -- ^ Do not perform this substitution, but keep going if in
    -- global mode.
    | SubCalloutAbort -- ^ Do not perform this or any subsequent substitutions.
    deriving (Show, Eq)

-- ** Lower-level representation of options and C data

type CompileContext = ForeignPtr Pcre2_compile_context
type Code           = ForeignPtr Pcre2_code
type MatchContext   = ForeignPtr Pcre2_match_context
type MatchData      = ForeignPtr Pcre2_match_data

-- | An `Option` can result in multiple \"plans\".
applyOption :: Option -> [AppliedOption]
applyOption = \case
    NoOptions            -> []
    TwoOptions opt0 opt1 -> applyOption opt0 ++ applyOption opt1

    -- CompileOption
    AllowEmptyClass   -> [CompileOption pcre2_ALLOW_EMPTY_CLASS]
    AltBsuxLegacy     -> [CompileOption pcre2_ALT_BSUX]
    AltCircumflex     -> [CompileOption pcre2_ALT_CIRCUMFLEX]
    AltVerbNames      -> [CompileOption pcre2_ALT_VERBNAMES]
    Anchored          -> [CompileOption pcre2_ANCHORED]
    AutoCallout       -> [CompileOption pcre2_AUTO_CALLOUT]
    Caseless          -> [CompileOption pcre2_CASELESS]
    DollarEndOnly     -> [CompileOption pcre2_DOLLAR_ENDONLY]
    DotAll            -> [CompileOption pcre2_DOTALL]
    EndAnchored       -> [CompileOption pcre2_ENDANCHORED]
    Extended          -> [CompileOption pcre2_EXTENDED]
    ExtendedMore      -> [CompileOption pcre2_EXTENDED_MORE]
    FirstLine         -> [CompileOption pcre2_FIRSTLINE]
    Literal           -> [CompileOption pcre2_LITERAL]
    MatchUnsetBackRef -> [CompileOption pcre2_MATCH_UNSET_BACKREF]
    Multiline         -> [CompileOption pcre2_MULTILINE]
    NeverBackslashC   -> [CompileOption pcre2_NEVER_BACKSLASH_C]
    NeverUcp          -> [CompileOption pcre2_NEVER_UCP]
    NoAutoCapture     -> [CompileOption pcre2_NO_AUTO_CAPTURE]
    NoAutoPossess     -> [CompileOption pcre2_NO_AUTO_POSSESS]
    NoDotStarAnchor   -> [CompileOption pcre2_NO_DOTSTAR_ANCHOR]
    NoStartOptimize   -> [CompileOption pcre2_NO_START_OPTIMIZE]
    Ucp               -> [CompileOption pcre2_UCP]
    Ungreedy          -> [CompileOption pcre2_UNGREEDY]

    -- CompileExtraOption
    AltBsux            -> [CompileExtraOption pcre2_EXTRA_ALT_BSUX]
    BadEscapeIsLiteral -> [CompileExtraOption pcre2_EXTRA_BAD_ESCAPE_IS_LITERAL]
    EscapedCrIsLf      -> [CompileExtraOption pcre2_EXTRA_ESCAPED_CR_IS_LF]
    MatchLine          -> [CompileExtraOption pcre2_EXTRA_MATCH_LINE]
    MatchWord          -> [CompileExtraOption pcre2_EXTRA_MATCH_WORD]

    -- CompileContextOption
    Bsr bsr -> unary
        CompileContextOption pcre2_set_bsr (bsrToC bsr)
    MaxPatternLength len -> unary
        CompileContextOption pcre2_set_max_pattern_length (fromIntegral len)
    Newline newline -> unary
        CompileContextOption pcre2_set_newline (newlineToC newline)
    ParensLimit limit -> unary
        CompileContextOption pcre2_set_parens_nest_limit (fromIntegral limit)

    -- CompileRecGuardOption
    UnsafeCompileRecGuard f -> [CompileRecGuardOption f]

    -- MatchOption
    NotBol             -> [MatchOption pcre2_NOTBOL]
    NotEmpty           -> [MatchOption pcre2_NOTEMPTY]
    NotEmptyAtStart    -> [MatchOption pcre2_NOTEMPTY_ATSTART]
    NotEol             -> [MatchOption pcre2_NOTEOL]
    PartialHard        -> [MatchOption pcre2_PARTIAL_HARD]
    PartialSoft        -> [MatchOption pcre2_PARTIAL_SOFT]
    SubGlobal          -> [MatchOption pcre2_SUBSTITUTE_GLOBAL]
    SubLiteral         -> [MatchOption pcre2_SUBSTITUTE_LITERAL]
    SubReplacementOnly -> [MatchOption pcre2_SUBSTITUTE_REPLACEMENT_ONLY]
    SubUnknownUnset    -> [MatchOption pcre2_SUBSTITUTE_UNKNOWN_UNSET]
    SubUnsetEmpty      -> [MatchOption pcre2_SUBSTITUTE_UNSET_EMPTY]

    -- CalloutOption
    UnsafeCallout f -> [CalloutOption f]

    -- SubCalloutOption
    UnsafeSubCallout f -> [SubCalloutOption f]

    -- MatchContextOption
    DepthLimit limit -> unary
        MatchContextOption pcre2_set_depth_limit (fromIntegral limit)
    HeapLimit limit -> unary
        MatchContextOption pcre2_set_heap_limit (fromIntegral limit)
    MatchLimit limit -> unary
        MatchContextOption pcre2_set_match_limit (fromIntegral limit)
    OffsetLimit limit -> CompileOption pcre2_USE_OFFSET_LIMIT : unary
        MatchContextOption pcre2_set_offset_limit (fromIntegral limit)

    where
    unary ctor f x = [ctor $ \ctx -> withForeignPtr ctx applyAndCheck] where
        applyAndCheck ctxPtr = f ctxPtr x >>= check (== 0)

-- | Intermediate representation of options expressing what effect they'll have
-- on which stage of regex compilation\/execution.  Also provide fake @Prism'@s.
data AppliedOption
    = CompileOption !CUInt
    | CompileExtraOption !CUInt
    | CompileContextOption !(CompileContext -> IO ())
    | CompileRecGuardOption !(Int -> IO Bool)
    | MatchOption !CUInt
    | CalloutOption !(CalloutInfo -> IO CalloutResult)
    | SubCalloutOption !(SubCalloutInfo -> IO SubCalloutResult)
    | MatchContextOption !(MatchContext -> IO ())

_CompileOption f =
    \case CompileOption x -> CompileOption <$> f x; o -> pure o
_CompileExtraOption f =
    \case CompileExtraOption x -> CompileExtraOption <$> f x; o -> pure o
_CompileContextOption f =
    \case CompileContextOption x -> CompileContextOption <$> f x; o -> pure o
_CompileRecGuardOption f =
    \case CompileRecGuardOption x -> CompileRecGuardOption <$> f x; o -> pure o
_MatchOption f =
    \case MatchOption x -> MatchOption <$> f x; o -> pure o
_CalloutOption f =
    \case CalloutOption x -> CalloutOption <$> f x; o -> pure o
_SubCalloutOption f =
    \case SubCalloutOption x -> SubCalloutOption <$> f x; o -> pure o
_MatchContextOption f =
    \case MatchContextOption x -> MatchContextOption <$> f x; o -> pure o

-- ** Extracting options at the right times
--
-- $ExtractingOptionsAtTheRightTimes
-- We interleave the extraction of options with the manipulation of foreign data
-- en route to the target `Matcher` or `Subber`.

-- | A `Monad` modeling both option extraction and foreign effects.
type ExtractOpts = StateT [AppliedOption] IO

-- | Use a fake @Prism'@ to extract a category of options.
extractOptsOf :: Getting (First a) AppliedOption a -> ExtractOpts [a]
extractOptsOf prism = state $ partitionEithers . map discrim where
    discrim opt = maybe (Right opt) Left $ opt ^? prism

-- | Prepare to compile a `Code`.
extractCompileEnv :: ExtractOpts CompileEnv
extractCompileEnv = do
    ctxUpds <- extractOptsOf _CompileContextOption
    xtraOpts <- bitOr <$> extractOptsOf _CompileExtraOption
    recGuard <- preview _last <$> extractOptsOf _CompileRecGuardOption

    if null ctxUpds && xtraOpts == 0 && null recGuard
        then return CompileEnv{
            compileEnvCtx  = Nothing,
            compileEnvERef = Nothing}

        else liftIO $ do
            ctx <- mkForeignPtr pcre2_compile_context_free $
                pcre2_compile_context_create nullPtr

            forM_ ctxUpds $ \update -> update ctx

            when (xtraOpts /= 0) $ withForeignPtr ctx $ \ctxPtr ->
                pcre2_set_compile_extra_options ctxPtr xtraOpts >>= check (== 0)

            compileEnvERef <- forM recGuard $ \f -> do
                eRef <- newIORef Nothing
                funPtr <- mkFunPtr ctx $ mkRecursionGuard $ \depth _ -> do
                    resultOrE <- try $ f (fromIntegral depth) >>= evaluate
                    case resultOrE of
                        Right success -> return $ if success then 0 else 1
                        Left e        -> writeIORef eRef (Just e) >> return 1
                withForeignPtr ctx $ \ctxPtr ->
                    pcre2_set_compile_recursion_guard ctxPtr funPtr nullPtr >>=
                        check (== 0)
                return eRef

            return CompileEnv{compileEnvCtx = Just ctx, ..}

-- | Inputs to `Code` compilation besides the pattern.
data CompileEnv = CompileEnv{
    compileEnvCtx :: !(Maybe CompileContext),
    -- | A register for catching exceptions thrown in recursion guards, if
    -- needed.
    compileEnvERef :: !(Maybe (IORef (Maybe SomeException)))}

-- | Compile a `Code`.
extractCode :: Text -> CompileEnv -> ExtractOpts Code
extractCode patt CompileEnv{..} = do
    opts <- bitOr <$> extractOptsOf _CompileOption

    liftIO $ mkForeignPtr pcre2_code_free $
        Text.useAsPtr patt $ \pattPtr pattCUs ->
        alloca $ \errorCodePtr ->
        alloca $ \errorOffPtr ->
        withForeignOrNullPtr compileEnvCtx $ \ctxPtr -> do
            codePtr <- pcre2_compile
                (toCUs pattPtr)
                (fromIntegral pattCUs)
                (opts .|. pcre2_UTF)
                errorCodePtr
                errorOffPtr
                ctxPtr
            when (codePtr == nullPtr) $ do
                -- Re-throw exception (if any) from recursion guard (if any)
                forM_ compileEnvERef $ readIORef >=> mapM_ throwIO
                -- Otherwise throw PCRE2 error
                errorCode <- peek errorCodePtr
                offCUs <- peek errorOffPtr
                throwIO $ Pcre2CompileException errorCode patt offCUs

            return codePtr

-- | `Code` and auxiliary compiled data used in preparation for a match or
-- substitution.  This remains constant for the lifetime of a `Matcher` or
-- `Subber`.
data MatchEnv = MatchEnv{
    matchEnvCode       :: Code,
    matchEnvOpts       :: CUInt,
    matchEnvCtx        :: Maybe MatchContext,
    matchEnvCallout    :: Maybe (CalloutInfo -> IO CalloutResult),
    matchEnvSubCallout :: Maybe (SubCalloutInfo -> IO SubCalloutResult)}

-- | Prepare a matching function after compiling the underlying @pcre2_code@.
extractMatchEnv :: Code -> ExtractOpts MatchEnv
extractMatchEnv matchEnvCode = do
    matchEnvOpts <- bitOr <$> extractOptsOf _MatchOption

    matchEnvCtx <- extractOptsOf _MatchContextOption >>= \case
        []      -> return Nothing
        ctxUpds -> liftIO $ Just <$> do
            ctx <- mkForeignPtr pcre2_match_context_free $
                pcre2_match_context_create nullPtr
            forM_ ctxUpds $ \update -> update ctx
            return ctx

    matchEnvCallout <- preview _last <$> extractOptsOf _CalloutOption
    matchEnvSubCallout <- preview _last <$> extractOptsOf _SubCalloutOption

    return MatchEnv{..}

-- | Generate, from user-supplied `Option`s and pattern, a `MatchEnv` that can
-- be reused for matching or substituting.
userMatchEnv :: Option -> Text -> IO MatchEnv
userMatchEnv option patt = runStateT extractAll (applyOption option) <&> \case
    (matchEnv, []) -> matchEnv
    _              -> error "BUG! Options not fully extracted"
    where
    extractAll = extractCompileEnv >>= extractCode patt >>= extractMatchEnv

-- | A `MatchEnv` is sufficient to fully implement a matching function.
matcherWithEnv :: MatchEnv -> Matcher
matcherWithEnv matchEnv@MatchEnv{..} subject = StreamEffect $
    withForeignPtr matchEnvCode $ \codePtr ->
    withMatchDataFromCode codePtr $ \matchDataPtr ->
    Text.useAsPtr subject $ \subjPtr subjCUs -> do
        MatchTempEnv{..} <- mkMatchTempEnv matchEnv subject

        -- Loop over the subject, emitting match data until stopping.
        return $ fix1 0 $ \continue curOff -> do
            result <- liftIO $ withForeignOrNullPtr matchTempEnvCtx $ \ctxPtr ->
                pcre2_match
                    codePtr
                    (toCUs subjPtr)
                    (fromIntegral subjCUs)
                    curOff
                    matchEnvOpts
                    matchDataPtr
                    ctxPtr

            -- Handle no match and errors
            when (result == pcre2_ERROR_NOMATCH) StreamStop
            when (result == pcre2_ERROR_CALLOUT) $
                liftIO $ maybeRethrow matchTempEnvRef
            liftIO $ check (> 0) result

            streamYield matchDataPtr

            -- Determine next starting offset
            nextOff <- liftIO $ do
                ovecPtr <- pcre2_get_ovector_pointer matchDataPtr
                curOffEnd <- peekElemOff ovecPtr 1
                -- Prevent infinite loop upon empty match
                return $ max curOffEnd (curOff + 1)

            -- Handle end of subject
            when (nextOff > fromIntegral subjCUs) StreamStop

            continue nextOff

    where
    withMatchDataFromCode codePtr action = do
        matchData <- mkForeignPtr pcre2_match_data_free $
            pcre2_match_data_create_from_pattern codePtr nullPtr
        withForeignPtr matchData action

-- | Helper to generate public matching functions.
pureUserMatcher :: Option -> Text -> Matcher
pureUserMatcher option patt =
    matcherWithEnv $ unsafePerformIO $ userMatchEnv option patt

-- | A `Subber` works by first writing results to a reasonably-sized buffer.  If
-- we run out of room, PCRE2 allows us to simulate the rest of the substitution
-- without writing anything, in order to calculate how big the buffer actually
-- has to be.  In this event, we rerun the substitution with a new,
-- exactly-sized buffer.
--
-- One potential issue arising from two attempts is running effectful callouts
-- twice.  We mitigate this by skipping callouts the second time:
--
-- * all regular callouts, since they had run during the simulation, and
--
-- * those substitution callouts that had run the first time.
--
-- Therefore, the first time, log the substitution callout indexes that had run
-- along with their results, and replay the log the second time, returning those
-- same results without re-incurring effects.
subberWithEnv :: MatchEnv -> Text -> Subber
subberWithEnv firstMatchEnv@MatchEnv{..} replacement subject =
    withForeignPtr matchEnvCode $ \codePtr ->
    Text.useAsPtr subject $ \subjPtr subjCUs ->
    Text.useAsPtr replacement $ \replPtr replCUs ->
    with (fromIntegral initOutLen) $ \outLenPtr -> do
        let run :: CUInt -> Ptr Pcre2_match_context -> PCRE2_SPTR -> IO CInt
            run curOpts ctxPtr outBufPtr = pcre2_substitute
                codePtr
                (toCUs subjPtr)
                (fromIntegral subjCUs)
                0
                (matchEnvOpts .|. curOpts)
                nullPtr
                ctxPtr
                (toCUs replPtr)
                (fromIntegral replCUs)
                outBufPtr
                outLenPtr

            checkAndGetOutput :: CInt -> PCRE2_SPTR -> IO (CInt, Text)
            checkAndGetOutput 0      _         = return (0, subject)
            checkAndGetOutput result outBufPtr = do
                check (> 0) result
                outLen <- peek outLenPtr
                out <- Text.fromPtr (fromCUs outBufPtr) (fromIntegral outLen)
                return (result, out)

        MatchTempEnv{..} <- mkMatchTempEnv firstMatchEnv subject
        firstAttempt <- withForeignOrNullPtr matchTempEnvCtx $ \ctxPtr ->
            allocaArray initOutLen $ \outBufPtr -> do
                result <- run pcre2_SUBSTITUTE_OVERFLOW_LENGTH ctxPtr outBufPtr
                maybeRethrow matchTempEnvRef
                if result == pcre2_ERROR_NOMEMORY
                    then Left <$> case matchTempEnvRef of
                        Nothing  -> return IM.empty
                        Just ref -> calloutStateSubsLog <$> readIORef ref
                    else Right <$> checkAndGetOutput result outBufPtr

        case firstAttempt of
            Right resultAndOutput -> return resultAndOutput
            Left subsLog          -> do
                -- The output was bigger than we guessed.  Try again.
                computedOutLen <- fromIntegral <$> peek outLenPtr
                let finalMatchEnv = firstMatchEnv{
                        -- Do not run regular callouts again.
                        matchEnvCallout = Nothing,
                        -- Do not run any substitution callouts run previously.
                        matchEnvSubCallout = fastFwd <$> matchEnvSubCallout}
                    fastFwd f = \info ->
                        case subsLog IM.!? subCalloutSubsCount info of
                            Just result -> return result
                            Nothing     -> f info
                MatchTempEnv{..} <- mkMatchTempEnv finalMatchEnv subject
                withForeignOrNullPtr matchTempEnvCtx $ \ctxPtr ->
                    allocaArray computedOutLen $ \outBufPtr -> do
                        result <- run 0 ctxPtr outBufPtr
                        maybeRethrow matchTempEnvRef
                        checkAndGetOutput result outBufPtr

    where
    -- Guess the size of the output to be <= 2x that of the subject.
    initOutLen = Text.length subject * 2

-- | Helper to generate substitution function.  For consistency with
-- `pureUserMatcher`.
pureUserSubber :: Option -> Text -> Text -> Subber
pureUserSubber option patt =
    subberWithEnv $ unsafePerformIO $ userMatchEnv option patt

-- | Generate per-call data for @pcre2_match()@ etc., to accommodate callouts.
--
-- We need to save and inspect state that occurs in potentially concurrent
-- matches.  This means a new state ref for each match, which means a new
-- `FunPtr` to close on it, which means a new match context to set it to.
mkMatchTempEnv
    :: MatchEnv
    -> Text -- ^ Callout info requires access to the original subject.
    -> IO MatchTempEnv
mkMatchTempEnv MatchEnv{..} subject
    | null matchEnvCallout && null matchEnvSubCallout = return MatchTempEnv{
        matchTempEnvCtx = matchEnvCtx,
        matchTempEnvRef = Nothing}

    | otherwise = do
        stateRef <- newIORef CalloutState{
            calloutStateException = Nothing,
            calloutStateSubsLog   = IM.empty}

        ctx <- mkForeignPtr pcre2_match_context_free $ case matchEnvCtx of
            -- No pre-existing match context, so create one afresh.
            Nothing -> pcre2_match_context_create nullPtr
            -- Pre-existing match context, so copy it.
            Just ctx -> withForeignPtr ctx pcre2_match_context_copy

        -- Install C function pointers in the @pcre2_match_context@.  When
        -- dereferenced and called, they will force the user-supplied Haskell
        -- callout functions and their results, catching any exceptions and
        -- saving them.

        -- Install callout, if any
        forM_ matchEnvCallout $ \f -> do
            funPtr <- mkFunPtr ctx $ mkCallout $ \blockPtr _ -> do
                info <- getCalloutInfo subject blockPtr
                resultOrE <- try $ f info >>= evaluate
                case resultOrE of
                    Right result -> return $ case result of
                        CalloutProceed     -> 0
                        CalloutNoMatchHere -> 1
                        CalloutNoMatch     -> pcre2_ERROR_NOMATCH
                    Left e -> do
                        modifyIORef' stateRef $ _calloutStateException ?~ e
                        return pcre2_ERROR_CALLOUT
            withForeignPtr ctx $ \ctxPtr ->
                pcre2_set_callout ctxPtr funPtr nullPtr >>= check (== 0)

        -- Install substitution callout, if any
        forM_ matchEnvSubCallout $ \f -> do
            funPtr <- mkFunPtr ctx $ mkCallout $ \blockPtr _ -> do
                info <- getSubCalloutInfo subject blockPtr
                resultOrE <- try $ f info >>= evaluate
                case resultOrE of
                    Right result -> do
                        modifyIORef' stateRef $ over _calloutStateSubsLog $
                            IM.insert (subCalloutSubsCount info) result
                        return $ case result of
                            SubCalloutAccept ->  0
                            SubCalloutSkip   ->  1
                            SubCalloutAbort  -> -1
                    Left e -> do
                        modifyIORef' stateRef $ _calloutStateException ?~ e
                        return (-1)
            withForeignPtr ctx $ \ctxPtr ->
                pcre2_set_substitute_callout ctxPtr funPtr nullPtr >>=
                    check (== 0)

        return MatchTempEnv{
            matchTempEnvCtx = Just ctx,
            matchTempEnvRef = Just stateRef}

-- | Per-call data for @pcre2_match()@ etc.
data MatchTempEnv = MatchTempEnv{
    matchTempEnvCtx :: !(Maybe MatchContext),
    matchTempEnvRef :: !(Maybe (IORef CalloutState))}

-- | Data computed during callouts that will be stashed in an IORef and
-- inspected after @pcre2_match()@ or similar completes.  `Lens'`s included.
data CalloutState = CalloutState{
    calloutStateException :: !(Maybe SomeException),
    calloutStateSubsLog   :: !(IntMap SubCalloutResult)}

_calloutStateException f CalloutState{..} =
    f calloutStateException <&> \calloutStateException -> CalloutState{..}
_calloutStateSubsLog f CalloutState{..} =
    f calloutStateSubsLog <&> \calloutStateSubsLog -> CalloutState{..}

foreign import ccall "wrapper" mkRecursionGuard :: FfiWrapper
    (CUInt -> Ptr a -> IO CInt)

foreign import ccall "wrapper" mkCallout :: FfiWrapper
    (Ptr block -> Ptr a -> IO CInt)

-- | Within a callout, marshal the original subject and @pcre2_callout_block@
-- data to Haskell and present to the user function.  Ensure no pointers are
-- leaked!
getCalloutInfo :: Text -> Ptr Pcre2_callout_block -> IO CalloutInfo
getCalloutInfo calloutSubject blockPtr = do
    calloutIndex <- do
        str <- pcre2_callout_block_callout_string blockPtr
        if str == nullPtr
            then pcre2_callout_block_callout_number blockPtr >>= \case
                -- Auto callout
                255 -> liftM2 CalloutAuto pattPos itemLen where
                    pattPos = intVia pcre2_callout_block_pattern_position
                    itemLen = intVia pcre2_callout_block_next_item_length
                    intVia getter = fromIntegral <$> getter blockPtr
                -- Numerical callout
                number -> return $ CalloutNumber $ fromIntegral number
            else do
                -- String callout
                len <- pcre2_callout_block_callout_string_length blockPtr
                CalloutName <$> Text.fromPtr (fromCUs str) (fromIntegral len)

    calloutCaptures <- do
        ovecPtr <- pcre2_callout_block_offset_vector blockPtr
        top <- pcre2_callout_block_capture_top blockPtr
        forM (0 :| [1 .. fromIntegral top - 1]) $ \n -> do
            (start, end) <- forOf each (0, 1) $ \i ->
                fromIntegral <$> peekElemOff ovecPtr (n * 2 + i)
            evaluate $ maybeSmartSlice calloutSubject $ Slice start end

    calloutMark <- do
        ptr <- pcre2_callout_block_mark blockPtr
        if ptr == nullPtr
            then return Nothing
            else Just <$> do
                len <- lengthArray0 0 ptr
                Text.fromPtr (fromCUs ptr) (fromIntegral len)

    flags <- pcre2_callout_block_callout_flags blockPtr
    let calloutIsFirst = flags .&. pcre2_CALLOUT_STARTMATCH /= 0
        calloutBacktracked = flags .&. pcre2_CALLOUT_BACKTRACK /= 0

    return CalloutInfo{..}

-- | Within a substitution callout, marshal the original subject and
-- @pcre2_substitute_callout_block@ data to Haskell and present to the user
-- function.  Ensure no pointers are leaked!
getSubCalloutInfo
    :: Text -> Ptr Pcre2_substitute_callout_block -> IO SubCalloutInfo
getSubCalloutInfo subCalloutSubject blockPtr = do
    subCalloutSubsCount <-
        fromIntegral <$> pcre2_substitute_callout_block_subscount blockPtr

    subCalloutCaptures <- do
        ovecPtr <- pcre2_substitute_callout_block_ovector blockPtr
        ovecCount <- pcre2_substitute_callout_block_oveccount blockPtr
        forM (0 :| [1 .. fromIntegral ovecCount - 1]) $ \n -> do
            (start, end) <- forOf each (0, 1) $ \i ->
                fromIntegral <$> peekElemOff ovecPtr (n * 2 + i)
            evaluate $ maybeSmartSlice subCalloutSubject $ Slice start end

    subCalloutReplacement <- do
        outPtr <- pcre2_substitute_callout_block_output blockPtr
        offsetsPtr <- pcre2_substitute_callout_block_output_offsets blockPtr
        (start, end) <- forOf each (0, 1) $ peekElemOff offsetsPtr
        Text.fromPtr
            (fromCUs $ advancePtr outPtr $ fromIntegral start)
            (fromIntegral $ end - start)

    return SubCalloutInfo{..}

-- | If there was a callout and it threw an exception, rethrow it.
maybeRethrow :: Maybe (IORef CalloutState) -> IO ()
maybeRethrow = mapM_ $ readIORef >=> mapM_ throwIO . calloutStateException

-- * Packaging @Matcher@s and @Subber@s as public API functions

-- | The most general form of a matching function, which can also be used as a
-- @Setter'@ to perform substitutions at the Haskell level.
_gcaptures :: (Traversable t) =>
    Matcher -> FromMatch t -> Traversal' Text (t Text)
_gcaptures matcher fromMatch f subject = traverse f captureTs <&> \captureTs' ->
    -- Swag foldl-as-foldr to create only as many segments as we need to stitch
    -- back together and no more.
    let beforeAndAfter = zip
            (concatMap toList sliceAndCaptureTs)
            (concatMap toList captureTs')
    in Text.concat $ foldr mkSegments termSegments beforeAndAfter 0

    where
    sliceAndCaptureTs = unsafeLazyStreamToList $
        mapMS (fromMatch >=> mapM enrichWithCapture) $ matcher subject
    enrichWithCapture slice = do
        capture <- evaluate $ smartSlice subject slice
        return (slice, capture)
    captureTs = map (snd <$>) sliceAndCaptureTs

    mkSegments ((Slice off offEnd, c), c') r prevOffEnd
        | off == fromIntegral pcre2_UNSET || c == c' =
            -- This substring is unset or unchanged.  Keep going without making
            -- cuts.
            r prevOffEnd
        | otherwise =
            -- Emit the subject up until here, and the new substring, and keep
            -- going, remembering where we are now.
            thinSlice subject (Slice prevOffEnd off) : c' : r offEnd
    termSegments off =
        let offEnd = fromIntegral $ Text.length subject
        -- If the terminal segment is empty, omit it altogether.  That way,
        -- Text.concat can just return the subject without copying anything in
        -- cases where no substring is changed.
        in [thinSlice subject (Slice off offEnd) | off /= offEnd]

-- | A function that takes a C match result and extracts captures into a
-- container.  We need to pass this effectful callback to `_gcaptures` because
-- of the latter's imperative loop that reuses the same @pcre2_match_data@
-- block.
--
-- The container type is polymorphic and in practice carries a `Traversable`
-- constraint.  Currently the following containers are used:
--
-- * `NonEmpty` when we want multiple capture groups;
--
-- * `Identity` when we only want the 0th;
--
-- * `Proxy` when we are only checking if a match succeeded;
--
-- * @[]@ when we want to easily pattern match specific capture groups via
--   Template Haskell-generated @ViewPatterns@.
type FromMatch t = Ptr Pcre2_match_data -> IO (t Slice)

-- | Read all specifically indexed captures' offsets from match results.
getWhitelistedSlices :: (Traversable t) => t Int -> FromMatch t
getWhitelistedSlices whitelist matchDataPtr = do
    ovecPtr <- pcre2_get_ovector_pointer matchDataPtr
    let peekOvec :: Int -> IO Text.I8
        peekOvec = fmap fromIntegral . peekElemOff ovecPtr

    forM whitelist $ \i -> Slice
        <$> peekOvec (i * 2)
        <*> peekOvec (i * 2 + 1)

-- | Read just the 0th capture's offsets from match results.
get0thSlice :: FromMatch Identity
get0thSlice = getWhitelistedSlices $ Identity 0

-- | Read all captures' offsets from match results.
getAllSlices :: FromMatch NonEmpty
getAllSlices matchDataPtr = do
    count <- fromIntegral <$> pcre2_get_ovector_count matchDataPtr
    let whitelist = 0 :| [1 .. count - 1]

    getWhitelistedSlices whitelist matchDataPtr

-- | Placeholder for building a `Traversal'` to be passed to `has`.
getNoSlices :: FromMatch Proxy
getNoSlices _ = return Proxy

-- | Match a pattern to a subject and return some non-empty list(s) of captures
-- in an `Alternative`, or `empty` if no match.  The non-empty list constructor
-- `:|` serves as a cue to differentiate the 0th capture from the others:
--
-- > let parseDate = captures "(\\d{4})-(\\d{2})-(\\d{2})"
-- > in case parseDate "submitted 2020-10-20" of
-- >     Just (date :| [y, m, d]) -> ...
-- >     Nothing                  -> putStrLn "didn't match"
--
-- @since 2.0.0
captures :: (Alternative f) => Text -> Text -> f (NonEmpty Text)
captures = capturesOpt mempty

-- | @capturesOpt mempty = captures@
--
-- @since 2.0.0
capturesOpt :: (Alternative f) => Option -> Text -> Text -> f (NonEmpty Text)
capturesOpt option patt = toAlternativeOf $ _capturesOpt option patt

-- | Does the pattern match the subject at least once?
matches :: Text -> Text -> Bool
matches = matchesOpt mempty

-- | @matchesOpt mempty = matches@
matchesOpt :: Option -> Text -> Text -> Bool
matchesOpt option patt = has $
    _gcaptures (pureUserMatcher option patt) getNoSlices

-- | Match a pattern to a subject and return the portion(s) that matched in an
-- `Alternative`, or `empty` if no match.
--
-- @since 2.0.0
match :: (Alternative f) => Text -> Text -> f Text
match = matchOpt mempty

-- | @matchOpt mempty = match@
--
-- @since 2.0.0
matchOpt :: (Alternative f) => Option -> Text -> Text -> f Text
matchOpt option patt = toAlternativeOf $ _matchOpt option patt

-- | Perform at most one substitution.  See
-- [the docs](https://pcre.org/current/doc/html/pcre2api.html#SEC36) for the
-- special syntax of /replacement/.
--
-- >>> sub "(\\w+) calling the (\\w+)" "$2 calling the $1" "the pot calling the kettle black"
-- "the kettle calling the pot black"
sub
    :: Text -- ^ pattern
    -> Text -- ^ replacement
    -> Text -- ^ subject
    -> Text -- ^ result
sub = subOpt mempty

-- | Perform substitutions globally.
--
-- >>> gsub "a" "o" "apples and bananas"
-- "opples ond bononos"
gsub :: Text -> Text -> Text -> Text
gsub = subOpt SubGlobal

-- | @
-- subOpt mempty = sub
-- subOpt SubGlobal = gsub
-- @
subOpt :: Option -> Text -> Text -> Text -> Text
subOpt option patt replacement =
    snd . unsafePerformIO . pureUserSubber option patt replacement

-- | Given a pattern, produce a traversal (0 or more targets) that focuses from
-- a subject to each non-empty list of captures that pattern matches.
--
-- Substitution works in the following way:  If a capture is set such that the
-- new `Text` is not equal to the old one, a substitution occurs, otherwise it
-- doesn't.  This matters in cases where a capture encloses another
-- capture&#x2014;notably, /all/ parenthesized captures are enclosed by the 0th.
--
-- >>> threeAndMiddle = _captures ". (.) ."
-- >>> "A A A" & threeAndMiddle .~ "A A A" :| ["B"]
-- "A B A"
-- >>> "A A A" & threeAndMiddle .~ "A B A" :| ["A"]
-- "A B A"
--
-- Changing multiple overlapping captures won't do what you want and is
-- unsupported.
--
-- Changing an unset capture is unsupported because the PCRE2 match API does not
-- give location info about it.  Currently we ignore all such attempts.  (Native
-- substitution functions like `sub` do not have this limitation.  See also
-- `SubUnknownUnset` and `SubUnsetEmpty`.)
--
-- If the list becomes longer for some reason, the extra elements are ignored.
-- If it's shortened, the absent elements are considered to be unchanged.
--
-- It's recommended that the list be modified capture-wise, using `ix`.
--
-- > let madlibs = _captures "(\\w+) my (\\w+)"
-- >
-- > print $ "Well bust my buttons!" &~ do
-- >     zoom madlibs $ do
-- >         ix 1 . _head .= 'd'
-- >         ix 2 %= Text.reverse
-- >     _last .= '?'
-- >
-- > -- "Well dust my snottub?"
_captures :: Text -> Traversal' Text (NonEmpty Text)
_captures = _capturesOpt mempty

-- | @_capturesOpt mempty = _captures@
_capturesOpt :: Option -> Text -> Traversal' Text (NonEmpty Text)
_capturesOpt option patt = _gcaptures (pureUserMatcher option patt) getAllSlices

-- | Given a pattern, produce a traversal (0 or more targets) that focuses from
-- a subject to the non-overlapping portions of it that match.
--
-- Equivalent to @`_captures` patt . `ix` 0@, but more efficient.
_match :: Text -> Traversal' Text Text
_match = _matchOpt mempty

-- | @_matchOpt mempty = _match@
_matchOpt :: Option -> Text -> Traversal' Text Text
_matchOpt option patt =
    _gcaptures (pureUserMatcher option patt) get0thSlice . _Identity

-- * Exceptions

-- | The root of the PCRE2 exception hierarchy.
data SomePcre2Exception = forall e. (Exception e) => SomePcre2Exception !e
instance Show SomePcre2Exception where
    show (SomePcre2Exception e) = show e
instance Exception SomePcre2Exception

-- | Vanilla PCRE2 exceptions with messages generated by the underlying C
-- library.
newtype Pcre2Exception = Pcre2Exception CInt
instance Show Pcre2Exception where
    show (Pcre2Exception x) = "pcre2: " ++ Text.unpack (getErrorMessage x)
instance Exception Pcre2Exception where
    toException = toException . SomePcre2Exception
    fromException = fromException >=> \(SomePcre2Exception e) -> cast e

-- | PCRE2 compile exceptions.  Along with a message stating the cause, we show
-- the pattern with a cursor pointing at where the error is (if not after the
-- last character).
data Pcre2CompileException = Pcre2CompileException !CInt !Text !PCRE2_SIZE
instance Show Pcre2CompileException where
    show (Pcre2CompileException x patt offset) =
        "pcre2_compile: " ++ Text.unpack (getErrorMessage x) ++ "\n" ++
            concatMap (replicate tab ' ' ++) pattLinesMaybeWithCaret
        where
        tab = 20
        pattLinesMaybeWithCaret
            | offset' == Text.length patt = pattLines
            | otherwise                   = insertCaretLine numberedPattLines
        offset' = fromIntegral offset
        pattLines = unchompedLines $ Text.unpack patt
        numberedPattLines = zip [0 :: Int ..] pattLines
        (caretRow, caretCol) = (!! offset') $ do
            (row, line) <- numberedPattLines
            (col, _) <- zip [0 ..] line
            return (row, col)
        insertCaretLine = concatMap $ \(n, line) -> if
            | n /= caretRow     -> [line]
            | last line == '\n' -> [line, caretLine ++ "\n"]
            | otherwise         -> [line ++ "\n", caretLine]
        caretLine = replicate caretCol ' ' ++ "^"
instance Exception Pcre2CompileException where
    toException = toException . SomePcre2Exception
    fromException = fromException >=> \(SomePcre2Exception e) -> cast e

-- | Built-in message corresponding to the integer error code.
getErrorMessage :: CInt -> Text
getErrorMessage errorCode = unsafePerformIO $ do
    let bufCUs = 120
    allocaArray bufCUs $ \bufPtr -> do
        cus <- pcre2_get_error_message errorCode bufPtr (fromIntegral bufCUs)
        Text.fromPtr (fromCUs bufPtr) (fromIntegral cus)

-- | Most PCRE2 C functions return an @int@ indicating a possible error.  Test
-- it against a predicate, and throw an exception upon failure.
check :: (CInt -> Bool) -> CInt -> IO ()
check p = unless . p <*> throwIO . Pcre2Exception

-- * PCRE2 compile-time config

-- | Helper for getting PCRE2 compile-time config integers.
getConfigNumeric :: CUInt -> CUInt
getConfigNumeric what = unsafePerformIO $ alloca $ \ptr -> do
    pcre2_config what ptr
    peek ptr

-- | Helper for getting PCRE2 compile-time config strings.
getConfigString :: CUInt -> Maybe Text
getConfigString what = unsafePerformIO $ do
    len <- pcre2_config what nullPtr
    if len == pcre2_ERROR_BADOPTION
        then return Nothing
        -- FIXME Do we really need "+ 1" here?
        -- FIXME allocaBytes looks wrong
        else allocaBytes (fromIntegral (len + 1) * 2) $ \ptr -> do
            pcre2_config what ptr
            Just <$> Text.fromPtr ptr (fromIntegral len - 1)

-- | See t`Bsr`.
defaultBsr :: Bsr
defaultBsr = bsrFromC $ getConfigNumeric pcre2_CONFIG_BSR

-- | Which code widths PCRE2 is compiled to operate on.  Can be any combination
-- of 8, 16, and 32.  Should be @[8]@ but provided here for completeness.
compiledWidths :: [Int]
compiledWidths =
    let bitmap = getConfigNumeric pcre2_CONFIG_COMPILED_WIDTHS
    in [w | (b, w) <- [(1, 8), (2, 16), (4, 32)], b .&. bitmap /= 0]

-- | See `DepthLimit`.
defaultDepthLimit :: Int
defaultDepthLimit = fromIntegral $ getConfigNumeric pcre2_CONFIG_DEPTHLIMIT

-- | See `HeapLimit`.
defaultHeapLimit :: Int
defaultHeapLimit = fromIntegral $ getConfigNumeric pcre2_CONFIG_HEAPLIMIT

-- | Was PCRE2 built with JIT support?
supportsJit :: Bool
supportsJit = getConfigNumeric pcre2_CONFIG_JIT == 1

-- | A nice description of the CPU architecture JIT support is compiled for, if
-- any.
jitTarget :: Maybe Text
jitTarget = getConfigString pcre2_CONFIG_JITTARGET

-- | Number of bytes used for internal linkage in compiled regexes.
linkSize :: Int
linkSize = fromIntegral $ getConfigNumeric pcre2_CONFIG_LINKSIZE

-- | See `MatchLimit`.
defaultMatchLimit :: Int
defaultMatchLimit = fromIntegral $ getConfigNumeric pcre2_CONFIG_MATCHLIMIT

-- | See t`Newline`.
defaultNewline :: Newline
defaultNewline = newlineFromC $ getConfigNumeric pcre2_CONFIG_NEWLINE

-- | See `NeverBackslashC`.
defaultIsNeverBackslashC :: Bool
defaultIsNeverBackslashC = getConfigNumeric pcre2_CONFIG_NEVER_BACKSLASH_C == 1

-- | See `ParensLimit`.
defaultParensLimit :: Int
defaultParensLimit = fromIntegral $ getConfigNumeric pcre2_CONFIG_PARENSLIMIT

-- | Size in bytes of PCRE2's built-in character processing tables.
defaultTablesLength :: Int
defaultTablesLength = fromIntegral $ getConfigNumeric pcre2_CONFIG_TABLES_LENGTH

-- | Unicode version string such as @8.0.0@, if Unicode is supported at all.
unicodeVersion :: Maybe Text
unicodeVersion = case getConfigString pcre2_CONFIG_UNICODE_VERSION of
    Just v | Text.unpack v == "Unicode not supported" -> Nothing
    maybeV                                            -> maybeV

-- | Was PCRE2 built with Unicode support?
supportsUnicode :: Bool
supportsUnicode = getConfigNumeric pcre2_CONFIG_UNICODE == 1

-- | Version of the built-in C library.  The versioning scheme is that PCRE
-- legacy is 8.x and PCRE2 is 10.x, so this should be @10.@/something/.
pcreVersion :: Text
pcreVersion = case getConfigString pcre2_CONFIG_VERSION of
    Just v  -> v
    Nothing -> error "pcreVersion: unable to get string"
