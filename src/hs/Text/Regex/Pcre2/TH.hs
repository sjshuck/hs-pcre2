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
import qualified Data.IntMap.Strict         as IM
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as Map
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Foreign          as Text
import           Data.Type.Bool             (If)
import           Data.Type.Equality         (type (==))
import           Foreign
import           Foreign.C                  (CUInt)
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
    CaptNum (num :: Nat) '(hi, _) = If (num `CmpNat` hi == 'GT)
        -- then
        (TypeError (TypeLits.Text "No capture numbered " :<>: ShowType num))
        -- else
        num

    CaptNum (name :: Symbol) '(_, '(name, num) ': _) = num
    CaptNum (name :: Symbol) '(hi, _ ': kvs) = CaptNum name '(hi, kvs)
    CaptNum (name :: Symbol) _ = TypeError
        (TypeLits.Text "No capture named " :<>: ShowType name)

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

-- | From options and pattern, determine parenthesized captures' names in order.
predictCaptureNames :: Option -> Text -> IO [Maybe Text]
predictCaptureNames option patt = do
    code <- evalStateT
        (extractCompileEnv >>= extractCode patt)
        (applyOption option)

    withForeignPtr code getCaptureNames

-- | Get parenthesized captures' names in order.
getCaptureNames :: Ptr Pcre2_code -> IO [Maybe Text]
getCaptureNames codePtr = do
    nameCount <- getCodeInfo @CUInt codePtr pcre2_INFO_NAMECOUNT
    nameEntrySize <- getCodeInfo @CUInt codePtr pcre2_INFO_NAMEENTRYSIZE
    nameTable <- getCodeInfo @PCRE2_SPTR codePtr pcre2_INFO_NAMETABLE

    -- Can't do [0 .. nameCount - 1] because it underflows when nameCount == 0
    let indexes = takeWhile (< nameCount) [0 ..]
    names <- fmap IM.fromList $ forM indexes $ \i -> do
        let entryPtr = nameTable `advancePtr` fromIntegral (i * nameEntrySize)
            groupNamePtr = entryPtr `advancePtr` 1
        groupNumber <- peek entryPtr
        groupNameLen <- lengthArray0 0 groupNamePtr
        groupName <- Text.fromPtr
            (fromCUs groupNamePtr)
            (fromIntegral groupNameLen)
        return (fromIntegral groupNumber, groupName)

    hiCaptNum <- getCodeInfo @CUInt codePtr pcre2_INFO_CAPTURECOUNT

    return $ map (names IM.!?) [1 .. fromIntegral hiCaptNum]

-- | Low-level access to compiled pattern info, per the docs.
getCodeInfo :: (Storable a) => Ptr Pcre2_code -> CUInt -> IO a
getCodeInfo codePtr what = alloca $ \wherePtr -> do
    pcre2_pattern_info codePtr what wherePtr >>= check (== 0)
    peek wherePtr

-- | Predict parenthesized captures (maybe named) of a pattern at splice time.
predictCaptureNamesQ :: String -> Q [Maybe Text]
predictCaptureNamesQ = runIO . predictCaptureNames mempty . Text.pack

-- | Get the indexes of `Just` the named captures.
toKVs :: [Maybe Text] -> [(Int, Text)]
toKVs names = [(number, name) | (number, Just name) <- zip [1 ..] names]

-- | Generate the data-kinded phantom type parameter of `Captures` of a pattern,
-- if needed.
capturesInfoQ :: String -> Q (Maybe Type)
capturesInfoQ s = predictCaptureNamesQ s >>= \case
    -- No parenthesized captures, so need for Captures, so no info.
    [] -> return Nothing

    -- One or more parenthesized captures.  Present
    --     [Just "foo", Nothing, Nothing, Just "bar", Nothing]
    -- as
    --     '(5, '[ '("foo", 1), '("bar", 4)]).
    captureNames -> Just <$> promotedTupleT 2 `appT` hiQ `appT` kvsQ where
        -- 5
        hiQ = litT $ numTyLit $ fromIntegral $ length captureNames
        -- '[ '("foo", 1), '("bar", 4)]
        kvsQ = foldr f promotedNilT $ toKVs captureNames
        -- '("foo", 1) ': ...
        f (number, name) r = promotedConsT `appT` kvQ `appT` r where
            kvQ = promotedTupleT 2                           -- '(,)
                `appT` litT (strTyLit $ Text.unpack name)    -- "foo"
                `appT` litT (numTyLit $ fromIntegral number) -- 1

-- | Helper for `regex` with no parenthesized captures.
matchTH :: (Alternative f) => Text -> Text -> f Text
matchTH patt = toAlternativeOf $ _matchTH patt

-- | Helper for `regex` with parenthesized captures.
capturesTH :: forall info f. (Alternative f) =>
    Text -> Text -> f (Captures info)
capturesTH patt = toAlternativeOf $ _capturesTH patt

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
    quoteExp = \s -> do
        let regexQ = capturesInfoQ s >>= \case
                Nothing   -> [e| matchTH |]
                Just info -> [e| capturesTH |] `appTypeE` return info
        regexQ `appE` [e| Text.pack $(stringE s) |],

    quotePat = \s -> do
        captureNames <- predictCaptureNamesQ s

        case toKVs captureNames of
            -- No named captures.  Test whether the string matches without
            -- creating any new Text values.
            [] -> viewP
                [e| matchesTH (Text.pack $(stringE s)) |]
                [p| True |]

            -- One or more named captures.  Attempt to bind only those to local
            -- variables of the same names.
            numberedNames -> viewP e p where
                (numbers, names) = unzip numberedNames
                e = [e| capturesNumberedTH
                    (Text.pack $(stringE s))
                    $(liftData numbers) |]
                p = foldr f wildP names
                f name r = conP '(:) [varP $ mkName $ Text.unpack name, r],

    quoteType = const $ fail "regex: cannot produce a type",

    quoteDec = const $ fail "regex: cannot produce declarations"}

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
    quoteExp = \s -> do
        let _regexQ = capturesInfoQ s >>= \case
                Nothing   -> [e| _matchTH |]
                Just info -> [e| _capturesTH |] `appTypeE` return info
        _regexQ `appE` [e| Text.pack $(stringE s) |],

    quotePat = const $ fail "_regex: cannot produce a pattern",

    quoteType = const $ fail "_regex: cannot produce a type",

    quoteDec = const $ fail "_regex: cannot produce declarations"}
