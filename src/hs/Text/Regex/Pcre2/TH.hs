{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Pcre2.TH where

import           Control.Applicative        (Alternative(..))
import           Data.IORef
import           Data.Functor               ((<&>))
import           Data.List.NonEmpty         (NonEmpty)
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

globalMatcherCache :: IORef (Map Text Matcher)
globalMatcherCache = unsafePerformIO $ newIORef Map.empty

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

matcherQ :: String -> ExpQ
matcherQ s = [e| memoMatcher $ Text.pack $(stringE s) |]

predictCaptureNamesQ :: String -> Q [Maybe Text]
predictCaptureNamesQ = runIO . predictCaptureNames mempty . Text.pack

toKVs :: [Maybe Text] -> [(Int, Text)]
toKVs names = [(number, name) | (number, Just name) <- zip [0 ..] names]

capturesInfoQ :: String -> TypeQ
capturesInfoQ s = do
    -- [Nothing, Just "foo", Just "bar", Nothing]
    captureNames <- predictCaptureNamesQ s

    let -- 3
        hi = litT $ numTyLit $ fromIntegral $ length captureNames - 1
        -- '[ '("foo", 1), '("bar", 2)]
        kvs = foldr f promotedNilT $ toKVs captureNames where
            -- '("foo", 1) ': ...
            f (number, name) = appT $ appT promotedConsT $       -- ':
                promotedTupleT 2                                 -- '(,)
                    `appT` litT (strTyLit $ Text.unpack name)    -- "foo"
                    `appT` litT (numTyLit $ fromIntegral number) -- 1

    -- '(3, '[ '("foo", 1), '("bar", 2)])
    promotedTupleT 2 `appT` hi `appT` kvs

-- | When used as an expression, this effectively has the type
--
-- > regex :: (Alternative f) => String -> Text -> f (Captures info)
--
-- Thus, it is like `capturesA`, except instead of returning a plain @`NonEmpty`
-- `Text`@, it returns an opaque `Captures`.  To retrieve an individual capture
-- from this object, use `capture`.
--
-- > case [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] "submitted 2020-10-20" of
-- >     Just cs ->
-- >         let date = capture @0 cs
-- >             year = read @Int $ capture @"y" cs
-- >             ...
--
-- When used as a pattern, it matches when the regex matches, whereupon any
-- named captures are bound to variables of the same name.
--
-- > case "submitted 2020-10-20" of
-- >     [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] ->
-- >         let year = read @Int y
-- >             ...
--
-- Note that it is not possible to access the 0th capture this way.  As a
-- workaround, explicitly capture the whole pattern string and name it.  Unnamed
-- captures (including the 0th) do not cause `Text` values to be created
-- wastefully, so this will not incur a performance penalty.
--
-- If there are no named captures, the pattern simply acts as a guard and does
-- not bind to any variables.
regex :: QuasiQuoter
regex = QuasiQuoter {
    quoteExp = \s -> [e|
        let wrap cs = Captures cs :: Captures $(capturesInfoQ s)
            maybeCaptures = preview $ _capturesInternal $(matcherQ s) [0 ..]
        in maybe empty (pure . wrap) . maybeCaptures |],

    quotePat = \s -> do
        captureNames <- predictCaptureNamesQ s

        case toKVs captureNames of
            -- No named captures.  Test whether the string matches without
            -- creating any new Text values.
            [] -> viewP
                [e| has $ _capturesInternal $(matcherQ s) [] |]
                [p| True |]

            -- One or more named captures.  Attempt to bind only those to local
            -- variables of the same names.
            numberedNames -> viewP e p where
                (numbers, names) = unzip numberedNames
                whitelistQ = liftData numbers
                _csQ = [e| _capturesInternal $(matcherQ s) $(whitelistQ) |]
                e = [e| view $ $(_csQ) . to NE.toList |]
                p = listP $ map (varP . mkName . Text.unpack) names,

    quoteType = const $ fail "regex: cannot produce a type",

    quoteDec = const $ fail "regex: cannot produce declarations"}

-- | A lens variant of `regex`.  Can only be used as an expression.
--
-- > _regex :: String -> Traversal' Text (Captures info)
--
-- > import Control.Lens
-- > import Data.Text.Lens
-- >
-- > embeddedNumber :: Traversal' String Int
-- > embeddedNumber = packed . [_regex|\d+|] . _capture @0 . unpacked . _Show
-- >
-- > main :: IO ()
-- > main = putStrLn $ "There are 14 competing standards" & embeddedNumber %~ (+ 1)
-- >
-- > -- There are 15 competing standards
--
_regex :: QuasiQuoter
_regex = QuasiQuoter {
    quoteExp = \s -> [e|
        let wrapped :: Lens' (NonEmpty Text) (Captures $(capturesInfoQ s))
            wrapped f cs = f (Captures cs) <&> \(Captures cs') -> cs'
        in _capturesInternal $(matcherQ s) [0 ..] . wrapped |],

    quotePat = const $ fail "_regex: cannot produce a pattern",

    quoteType = const $ fail "_regex: cannot produce a type",

    quoteDec = const $ fail "_regex: cannot produce declarations"}
