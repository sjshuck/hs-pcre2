{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Pcre2.TH where

import           Control.Applicative        (Alternative(..))
import           Data.Functor               ((<&>))
import           Data.IORef
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NE
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Regex.Pcre2.Internal

-- | Unexported, top-level `IORef` that\'s created upon the first runtime
-- evaluation of a Template Haskell `Matcher`.
globalMatcherCache :: IORef (Map Text Matcher)
globalMatcherCache = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE globalMatcherCache #-}

-- | Given a `Text`, create or retrieve a `Matcher` from the global cache.
memoMatcher :: Text -> Matcher
memoMatcher patt = unsafePerformIO $ do
    cache <- readIORef globalMatcherCache
    case Map.lookup patt cache of
        Just matcher -> return matcher
        Nothing      -> do
            let matcher = unsafePerformIO $ assembleMatcher mempty patt
            atomicModifyIORef' globalMatcherCache $ \cache ->
                (Map.insert patt matcher cache, ())
            return matcher

-- | Generate code to produce \(and memoize\) a `Matcher` from a pattern.
matcherQ :: String -> ExpQ
matcherQ s = [e| memoMatcher $ Text.pack $(stringE s) |]

-- | Predict parenthesized captures \(maybe named\) of a pattern at splice time.
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
    --     [Just "foo", Just "bar", Nothing]
    -- as
    --     '(3, '[ '("foo", 1), '("bar", 2)]).
    captureNames -> Just <$> promotedTupleT 2 `appT` hi `appT` kvs where
        -- 3
        hi = litT $ numTyLit $ fromIntegral $ length captureNames
        -- '[ '("foo", 1), '("bar", 2)]
        kvs = foldr f promotedNilT $ toKVs captureNames where
            -- '("foo", 1) ': ...
            f (number, name) = appT $ appT promotedConsT $       -- ':
                promotedTupleT 2                                 -- '(,)
                    `appT` litT (strTyLit $ Text.unpack name)    -- "foo"
                    `appT` litT (numTyLit $ fromIntegral number) -- 1

-- | /__As an expression__/
--
-- > regex :: (Alternative f) => String -> Text -> f (Captures info)
--
-- in the presence of parenthesized captures, or
--
-- > regex :: (Alternative f) => String -> Text -> f Text
--
-- if there are none.  In other words, if there is more than the 0th capture,
-- this behaves like `capturesA` (except returning an opaque `Captures` instead
-- of a list), otherwise it behaves like `match`.
--
-- To retrieve an individual capture from a `Captures`, use `capture`.
--
-- > case [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] "submitted 2020-10-20" of
-- >     Just cs ->
-- >         let date = capture @0 cs
-- >             year = read @Int $ Text.unpack $ capture @"y" cs
-- >             ...
--
-- > forM_ ([regex|^\s+$|] line :: Maybe Text) $ \spaces ->
-- >     error $ "line has spaces only: " ++ show spaces
--
-- /__As a pattern__/
--
-- This matches when the regex matches, whereupon any named captures are bound
-- to variables of the same names.
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
regex = QuasiQuoter {
    quoteExp = \s -> capturesInfoQ s >>= \case
        Nothing -> [e|
            let _cs = _capturesInternal $(matcherQ s) get0thSliceRanges slice
            in toAlternativeOf $ _cs . _headNE |]
        Just info -> [e|
            let _cs = _capturesInternal $(matcherQ s) getAllSliceRanges slice
                wrap cs = Captures cs :: Captures $(return info)
            in toAlternativeOf $ _cs . to wrap |],

    quotePat = \s -> do
        captureNames <- predictCaptureNamesQ s

        case NE.nonEmpty $ toKVs captureNames of
            -- No named captures.  Test whether the string matches without
            -- creating any new Text values.
            Nothing -> viewP
                [e|
                    has $ _capturesInternal
                        $(matcherQ s)
                        (const $ return noTouchy)
                        noTouchy |]
                [p| True |]

            -- One or more named captures.  Attempt to bind only those to local
            -- variables of the same names.
            Just numberedNames -> viewP e p where
                (numbers, names) = NE.unzip numberedNames
                e = [e|
                    let _cs = _capturesInternal
                            $(matcherQ s)
                            (getWhitelistedSliceRanges $(liftData numbers))
                            slice
                    in view $ _cs . to NE.toList |]
                p = foldr f wildP names where
                    f name r = conP '(:) [varP $ mkName $ Text.unpack name, r],

    quoteType = const $ fail "regex: cannot produce a type",

    quoteDec = const $ fail "regex: cannot produce declarations"}

-- | A global, optical variant of `regex`.  Can only be used as an expression.
--
-- > _regex :: String -> Traversal' Text (Captures info)
-- > _regex :: String -> Traversal' Text Text
--
-- > import Control.Lens
-- > import Data.Text.Lens
-- >
-- > embeddedNumber :: Traversal' String Int
-- > embeddedNumber = packed . [_regex|\d+|] . unpacked . _Show
-- >
-- > main :: IO ()
-- > main = putStrLn $ "There are 14 competing standards" & embeddedNumber %~ (+ 1)
-- >
-- > -- There are 15 competing standards
--
_regex :: QuasiQuoter
_regex = QuasiQuoter {
    quoteExp = \s -> capturesInfoQ s >>= \case
        Nothing -> [e|
            let _cs = _capturesInternal $(matcherQ s) get0thSliceRanges slice
            in _cs . _headNE |]
        Just info -> [e|
            let _cs = _capturesInternal $(matcherQ s) getAllSliceRanges slice
                wrapped :: Lens' (NonEmpty Text) (Captures $(return info))
                wrapped f cs = f (Captures cs) <&> \(Captures cs') -> cs'
            in _cs . wrapped |],

    quotePat = const $ fail "_regex: cannot produce a pattern",

    quoteType = const $ fail "_regex: cannot produce a type",

    quoteDec = const $ fail "_regex: cannot produce declarations"}
