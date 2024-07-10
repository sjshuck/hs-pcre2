{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Regex.Pcre2.TH where

import           Control.Applicative        (Alternative(..))
import           Control.Monad              (forM)
import           Control.Monad.State.Strict (evalStateT)
import           Data.IORef
import           Data.List                  (sortBy)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as Map
import           Data.Ord                   (comparing)
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Foreign          as Text
import           Data.Type.Bool             (If)
import           Data.Type.Equality         (type (==))
import           Foreign
import           Foreign.C                  (CUChar, CUInt)
import           GHC.TypeLits               hiding (Text)
import qualified GHC.TypeLits               as TypeLits
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (liftData)
import           Lens.Micro
import           Lens.Micro.Extras          (view)
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Regex.Pcre2.Foreign
import           Text.Regex.Pcre2.Internal

-- | A wrapper around a list of captures that carries additional type-level
-- information about the number and names of those captures.
--
-- This type is only intended to be created by `regex`\/`_regex` and consumed by
-- `capture`\/`_capture`, relying on type inference.  Specifying the @info@
-- explicitly in a type signature is not supported&#x2014;the definition of
-- `CapturesInfo` is not part of the public API and may change without warning.
--
-- After obtaining `Captures` it's recommended to immediately consume them and
-- transform them into application-level data, to avoid leaking the types.
newtype Captures (info :: CapturesInfo) = Captures (NonEmpty Text)
    deriving (Show {- ^ @since 2.0.4 -})

-- | The kind of `Captures`'s @info@.  The first number is the total number of
-- parenthesized captures, and the list is a lookup table from capture names to
-- numbers.
type CapturesInfo = (Nat, [(Symbol, Nat)])

-- | Look up the number of a capture at compile time, either by number or by
-- name.  Throw a helpful 'TypeError' if the index doesn't exist.
type family CaptNum (i :: k) (info :: CapturesInfo) :: Nat where
    CaptNum (i :: Nat) '(hi, _) = If (i `CmpNat` hi == 'GT)
        -- then
        (TypeError (TypeLits.Text "No capture numbered " :<>: ShowType i))
        -- else
        i

    CaptNum (i :: Symbol) '(_,  '(i, num) ': _)   = num
    CaptNum (i :: Symbol) '(hi, _         ': kvs) = CaptNum i '(hi, kvs)
    CaptNum (i :: Symbol) _                       = TypeError
        (TypeLits.Text "No capture named " :<>: ShowType i)

    CaptNum _ _ = TypeError
        (TypeLits.Text "Capture index must be a number (Nat) or name (Symbol)")

-- | Safely lookup a capture in a `Captures` result obtained from a Template
-- Haskell-generated matching function.
--
-- The ugly type signature may be interpreted like this:  /Given some capture/
-- /group index @i@ and some @info@ about a regex, ensure that index exists and/
-- /is resolved to the number @num@ at compile time.  Then, at runtime, get a/
-- /capture group (numbered @num@) from a list of (at least @num@) captures./
--
-- In practice the variable @i@ is specified by type application and the other
-- variables are inferred.
--
-- > capture @3
-- > capture @"bar"
--
-- Specifying a nonexistent number or name will result in a type error.
capture :: forall i info num. (CaptNum i info ~ num, KnownNat num) =>
    Captures info -> Text
capture = view $ _capture @i

-- | Like `capture` but focus from a `Captures` to a capture.
_capture :: forall i info num. (CaptNum i info ~ num, KnownNat num) =>
    Lens' (Captures info) Text
_capture = _Captures . singular (ix $ fromInteger $ natVal @num Proxy) where
    _Captures f (Captures cs) = Captures <$> f cs

-- | Unexported, top-level `IORef` that's created upon the first runtime
-- evaluation of a Template Haskell `Matcher`.
globalMatcherCache :: IORef (Map Text Matcher)
globalMatcherCache = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE globalMatcherCache #-}

-- | Given a pattern, create or retrieve a `Matcher` from the global cache.
memoMatcher :: Text -> Matcher
memoMatcher patt = unsafePerformIO $ do
    cache <- readIORef globalMatcherCache
    case Map.lookup patt cache of
        Just matcher -> return matcher
        Nothing      -> atomicModifyIORef' globalMatcherCache $ \cache ->
            let matcher = pureUserMatcher mempty patt
            in (Map.insert patt matcher cache, matcher)

-- | From options and pattern, determine number of parenthesized captures, along
-- with a list of their names (each indexed and in index order).
predictCapturesInfo :: Option -> Text -> IO (Int, [(Text, Int)])
predictCapturesInfo option patt = do
    code <- evalStateT
        (extractCompileEnv >>= extractCode patt)
        (applyOption option)

    withForeignPtr code $ \codePtr -> do
        count <- getCodeInfo @CUInt codePtr pcre2_INFO_NAMECOUNT
        entrySize <- getCodeInfo @CUInt codePtr pcre2_INFO_NAMEENTRYSIZE
        table <- getCodeInfo @PCRE2_SPTR codePtr pcre2_INFO_NAMETABLE

        -- Can't do [0 .. count - 1] because it underflows when count == 0
        let indexes = takeWhile (< count) [0 ..]
        lookupTable <- forM indexes $ \i -> do
            let entryPtr = table `advancePtr` fromIntegral (i * entrySize)
                groupNamePtr = entryPtr `advancePtr` 2
            groupNumber <- do
                (hi, lo) <- forOf each (0, 1) $ \off ->
                    fromIntegral @CUChar <$> peekByteOff entryPtr off
                return $ hi `shiftL` 8 + lo
            groupNameLen <- lengthArray0 0 groupNamePtr
            groupName <- Text.fromPtr
                (fromCUs groupNamePtr)
                (fromIntegral groupNameLen)
            return (groupName, groupNumber)

        hiCaptNum <- getCodeInfo @CUInt codePtr pcre2_INFO_CAPTURECOUNT

        return (fromIntegral hiCaptNum, sortBy (comparing snd) lookupTable)

-- | Low-level access to compiled pattern info, per the docs.
getCodeInfo :: (Storable a) => Ptr Pcre2_code -> CUInt -> IO a
getCodeInfo codePtr what = alloca $ \wherePtr -> do
    pcre2_pattern_info codePtr what wherePtr >>= check (== 0)
    peek wherePtr

-- | Predict info about parenthesized captures of a pattern at splice time.
predictCapturesInfoQ :: String -> Q (Int, [(Text, Int)])
predictCapturesInfoQ = runIO . predictCapturesInfo mempty . Text.pack

-- | Generate the data-kinded phantom type parameter of `Captures` of a pattern,
-- if needed.
capturesInfoQ :: String -> Q (Maybe Type)
capturesInfoQ s = predictCapturesInfoQ s >>= \case
    -- No parenthesized captures, so need for Captures, so no info.
    (0, _) -> return Nothing

    -- One or more parenthesized captures.  Produce
    --     '(5, '[ '("foo", 1), '("bar", 4)]).
    (len, lookupTable) -> Just <$> promotedTupleT 2 `appT` hiQ `appT` kvsQ where
        -- 5
        hiQ = litT $ numTyLit $ fromIntegral len
        -- '[ '("foo", 1), '("bar", 4)]
        kvsQ = foldr f promotedNilT lookupTable
        -- '("foo", 1) ': ...
        f (name, number) r = promotedConsT `appT` kvQ `appT` r where
            kvQ = promotedTupleT 2                           -- '(,)
                `appT` litT (strTyLit $ Text.unpack name)    -- "foo"
                `appT` litT (numTyLit $ fromIntegral number) -- 1

-- | Helper for `regex` with no parenthesized captures.
matchTH :: (Alternative f) => Text -> Text -> f Text
matchTH patt = altOf $ _matchTH patt

-- | Helper for `regex` with parenthesized captures.
capturesTH :: forall info f. (Alternative f) =>
    Text -> Text -> f (Captures info)
capturesTH patt = altOf $ _capturesTH patt

-- | Helper for `regex` as a guard pattern.
matchesTH :: Text -> Text -> Bool
matchesTH patt = has $ _gcaptures (memoMatcher patt) getNoSlices

-- | Helper for `regex` as a pattern that binds local variables.
capturesNumberedTH :: Text -> [Int] -> Text -> [Text]
capturesNumberedTH patt numbers = view $
    _gcaptures (memoMatcher patt) (getWhitelistedSlices numbers)

-- | Helper for `_regex` with no parenthesized captures.
_matchTH :: Text -> Traversal' Text Text
_matchTH patt = _gcaptures (memoMatcher patt) get0thSlice . _Identity

-- | Helper for `_regex` with parenthesized captures.
_capturesTH :: Text -> Traversal' Text (Captures info)
_capturesTH patt = _gcaptures (memoMatcher patt) getAllSlices . captured where
    captured f cs = f (Captures cs) <&> \(Captures cs') -> cs'

-- | Helper to generate a `Text` in code.
textQ :: String -> ExpQ
textQ s = [e| Text.pack $(stringE s) |]

-- | Helper to generate a `quoteExp` function, which either simply matches the
-- subject or produces captures depending on the pattern.
mkQuoteExp :: ExpQ -> ExpQ -> String -> ExpQ
mkQuoteExp matchE capturesE s = regexQ `appE` textQ s where
    regexQ = capturesInfoQ s >>= \case
        Nothing   -> matchE
        Just info -> capturesE `appTypeE` return info

-- | === As an expression
--
-- @
-- regex :: (`Alternative` f) => String -> Text -> f (`Captures` info)
-- @
--
-- in the presence of parenthesized captures, or
--
-- > regex :: (Alternative f) => String -> Text -> f Text
--
-- if there are none.  In other words, if there is more than the 0th capture,
-- this behaves like `captures` (except returning an opaque `Captures` instead
-- of a `NonEmpty` list), otherwise it behaves like `Text.Regex.Pcre2.match`.
--
-- To retrieve an individual capture from a `Captures`, use `capture`.
--
-- > case [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] "submitted 2020-10-20" of
-- >     Just cs ->
-- >         let date = capture @0 cs
-- >             year = read @Int $ Text.unpack $ capture @"y" cs
-- >             ...
--
-- > forM_ ([regex|\s+$|] line :: Maybe Text) $ \spaces -> error $
-- >     "line has trailing spaces (" ++ show (Text.length spaces) ++ " characters)"
--
-- === As a pattern
--
-- This matches when the regex first matches, whereupon any named captures are
-- bound to variables of the same names.
--
-- > case "submitted 2020-10-20" of
-- >     [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] ->
-- >         let year = read @Int $ Text.unpack y
-- >             ...
--
-- Note that it is not possible to access the 0th capture this way.  As a
-- workaround, explicitly capture the whole pattern and name it.
--
-- If there are no named captures, this simply acts as a guard.
regex :: QuasiQuoter
regex = QuasiQuoter{
    quoteExp = mkQuoteExp [e| matchTH |] [e| capturesTH |],

    quotePat = \s -> predictCapturesInfoQ s >>= \info -> case snd info of
        []          -> viewP [e| matchesTH $(textQ s) |] [p| True |]
        lookupTable -> viewP e p where
            (names, numbers) = unzip lookupTable
            e = [e| capturesNumberedTH $(textQ s) $(liftData numbers) |]
            p = foldr f wildP names
            f name r = conP '(:) [varP $ mkName $ Text.unpack name, r],

    quoteType = \_ -> fail "regex: cannot produce a type",
    quoteDec  = \_ -> fail "regex: cannot produce declarations"}

-- | An optical variant of `regex`\/a type-annotated variant of `_captures`. Can
-- only be used as an expression.
--
-- @
-- _regex :: String -> `Traversal'` Text (`Captures` info)
-- _regex :: String -> Traversal' Text Text
-- @
--
-- > embeddedNumbers :: Traversal' String Int
-- > embeddedNumbers = packed . [_regex|\d+|] . unpacked . _Show
-- >
-- > main :: IO ()
-- > main = putStrLn $ "There are 14 competing standards" & embeddedNumbers %~ (+ 1)
-- >
-- > -- There are 15 competing standards
_regex :: QuasiQuoter
_regex = QuasiQuoter{
    quoteExp = mkQuoteExp [e| _matchTH |] [e| _capturesTH |],

    quotePat  = \_ -> fail "_regex: cannot produce a pattern",
    quoteType = \_ -> fail "_regex: cannot produce a type",
    quoteDec  = \_ -> fail "_regex: cannot produce declarations"}
