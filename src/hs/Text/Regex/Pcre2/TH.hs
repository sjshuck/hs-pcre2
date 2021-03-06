{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Pcre2.TH where

import           Control.Applicative        (Alternative(..))
import           Data.Functor               ((<&>))
import           Data.IORef
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NE
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as Map
import           Data.Proxy                 (Proxy(..))
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

-- | Given a pattern, create or retrieve a `Matcher` from the global cache.
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
    --     [Just "foo", Nothing, Nothing, Just "bar"]
    -- as
    --     '(4, '[ '("foo", 1), '("bar", 4), '("", 0)]).
    captureNames -> Just <$> promotedTupleT 2 `appT` hi `appT` kvs where
        -- 4
        hi = litT $ numTyLit $ fromIntegral $ length captureNames
        -- '[ '("foo", 1), '("bar", 4), '("", 0)]
        kvs = foldr f end (toKVs captureNames) where
            -- '("foo", 1) ': ...
            f (number, name) r = promotedConsT `appT` kv `appT` r where
                kv = promotedTupleT 2                            -- '(,)
                    `appT` litT (strTyLit $ Text.unpack name)    -- "foo"
                    `appT` litT (numTyLit $ fromIntegral number) -- 1
        -- FIXME GHC kind-checks empty '[] as [*] instead of [k], which breaks
        -- quasi-quoted regexes with parenthesized captures but no names.
        -- Therefore, we avoid ever splicing an empty lookup table by ending it
        -- with a placeholder entry (which cannot arise from a pattern since ""
        -- is an invalid capture group name).
        end = [t| '[ '("", 0)] |]

-- | Helper for `regex` with no parenthesized captures.
matchTH :: (Alternative f) => Text -> Text -> f Text
matchTH patt = toAlternativeOf $
    _capturesInternal (memoMatcher patt) get0thSlice . _Identity

-- | Helper for `regex` with parenthesized captures.
capturesTH :: (Alternative f) => Text -> Proxy info -> Text -> f (Captures info)
capturesTH patt _ = toAlternativeOf $
    _capturesInternal (memoMatcher patt) getAllSlices . to Captures

-- | Helper for `regex` as a guard pattern.
matchesTH :: Text -> Text -> Bool
matchesTH patt = has $ _capturesInternal (memoMatcher patt) nilFromMatch

-- | Helper for `regex` as a pattern that binds local variables.
capturesNumberedTH :: Text -> NonEmpty Int -> Text -> [Text]
capturesNumberedTH patt numbers = concatMap NE.toList . toListOf _cs where
    _cs = _capturesInternal (memoMatcher patt) fromMatch
    fromMatch = getWhitelistedSlices numbers

-- | Helper for `_regex` with no parenthesized captures.
_matchTH :: Text -> Traversal' Text Text
_matchTH patt = _capturesInternal (memoMatcher patt) get0thSlice . _Identity

-- | Helper for `_regex` with parenthesized captures.
_capturesTH :: Text -> Proxy info -> Traversal' Text (Captures info)
_capturesTH patt _ = _cs . wrapped where
    _cs = _capturesInternal (memoMatcher patt) getAllSlices
    wrapped f cs = f (Captures cs) <&> \(Captures cs') -> cs'

-- | === As an expression
--
-- > regex :: (Alternative f) => String -> Text -> f (Captures info)
--
-- in the presence of parenthesized captures, or
--
-- > regex :: (Alternative f) => String -> Text -> f Text
--
-- if there are none.  In other words, if there is more than the 0th capture,
-- this behaves like `captures` (except returning an opaque `Captures` instead
-- of a `NonEmpty` list), otherwise it behaves like `match`.
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
regex = QuasiQuoter {
    quoteExp = \s -> capturesInfoQ s >>= \case
        Nothing   -> [e| matchTH (Text.pack $(stringE s)) |]
        Just info -> [e| capturesTH
            (Text.pack $(stringE s))
            (Proxy :: Proxy $(return info)) |],

    quotePat = \s -> do
        captureNames <- predictCaptureNamesQ s

        case NE.nonEmpty $ toKVs captureNames of
            -- No named captures.  Test whether the string matches without
            -- creating any new Text values.
            Nothing -> viewP
                [e| matchesTH (Text.pack $(stringE s)) |]
                [p| True |]

            -- One or more named captures.  Attempt to bind only those to local
            -- variables of the same names.
            Just numberedNames -> viewP e p where
                (numbers, names) = NE.unzip numberedNames
                e = [e| capturesNumberedTH
                    (Text.pack $(stringE s))
                    $(liftData numbers) |]
                p = foldr f wildP names where
                    f name r = conP '(:) [varP $ mkName $ Text.unpack name, r],

    quoteType = const $ fail "regex: cannot produce a type",

    quoteDec = const $ fail "regex: cannot produce declarations"}

-- | An optical variant of `regex`.  Can only be used as an expression.
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
_regex :: QuasiQuoter
_regex = QuasiQuoter {
    quoteExp  = \s -> capturesInfoQ s >>= \case
        Nothing   -> [e| _matchTH (Text.pack $(stringE s)) |]
        Just info -> [e| _capturesTH
            (Text.pack $(stringE s))
            (Proxy :: Proxy $(return info)) |],

    quotePat = const $ fail "_regex: cannot produce a pattern",

    quoteType = const $ fail "_regex: cannot produce a type",

    quoteDec = const $ fail "_regex: cannot produce declarations"}
