{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Criterion.Main
import           Data.List.NonEmpty          (NonEmpty(..))
import qualified Data.Text                   as Text
import           Lens.Micro.Platform
import           System.IO.Unsafe            (unsafePerformIO)
import qualified Text.Regex.PCRE
import qualified Text.Regex.PCRE.Light.Char8
import qualified Text.Regex.PCRE.String
import qualified Text.Regex.PCRE.Text
import           Text.Regex.Pcre2

main :: IO ()
main = defaultMain [
    bgroup "creating single-use regexes" [
        bench "pcre2" $ flip nf strings $ \(patt, subj) ->
            let Just (_ :| [bar]) = capturesA (Text.pack patt) (Text.pack subj)
            in Text.unpack bar,

        bench "regex-pcre-builtin" $ flip nf strings $ \(patt, subj) ->
            let r = Text.Regex.PCRE.makeRegex patt :: Text.Regex.PCRE.Regex
                results :: (String, String, String)
                results@(_, result, _) = Text.Regex.PCRE.match r subj
            in result,

        bench "pcre-light" $ flip nf strings $ \(patt, subj) ->
            let r = Text.Regex.PCRE.Light.Char8.compile patt []
                Just [_, bar] = Text.Regex.PCRE.Light.Char8.match r subj []
            in bar],
    
    bgroup "strings" [
        let r = capturesA $ Text.pack stringPattern
        in bench "pcre2" $ flip nf stringSubject $ \subj ->
            let Just (_ :| [bar]) = r $ Text.pack subj in Text.unpack bar,

        let r = mkRegexBaseR stringPattern
        in bench "regex-pcre-builtin" $ nfIO $ do
            Right (Just (_, _, _, [bar])) <-
                Text.Regex.PCRE.String.regexec r stringSubject
            return bar,

        let r = Text.Regex.PCRE.Light.Char8.compile stringPattern []
        in bench "pcre-light" $ flip nf stringSubject $ \subj ->
            let Just [_, bar] = Text.Regex.PCRE.Light.Char8.match r subj []
            in bar],

    bgroup "texts" [
        bgroupTexts "short" strings,

        bgroupTexts "long" (
            "foo (ba*r) baz",
            "foo b" ++ replicate 1000 'a' ++ "r baz")],

    bgroup "Template Haskell" [
        bench "pattern with named capture" $ flip nf textSubject $ \subj ->
            let [regex|foo (?<bar>bar) baz|] = subj in bar,

        bench "expression with capture" $ flip nf textSubject $ \subj ->
            let Just cs = [regex|foo (?<bar>bar) baz|] subj
            in capture @"bar" cs],

    bgroup "substitutions" [
        bgroup "single" [
            bench "PCRE2-native" $ flip nf textSubject $
                sub (Text.pack "(?<=foo )bar(?= baz)") (Text.pack "quux"),

            let quux = Text.pack "quux"
            in bench "lens-powered" $ flip nf textSubject $
                set ([_regex|foo (bar) baz|] . _capture @1) quux],

        let fruit = Text.pack "apples and bananas"
        in bgroup "multiple" [
            let a2o = gsub (Text.pack "a") (Text.pack "o")
            in bench "PCRE2-native" $ nf a2o fruit,

            let a2o = set [_regex|a|] (Text.pack "o")
            in bench "lens-powered" $ nf a2o fruit]]]

stringPattern = "foo (bar) baz"
stringSubject = "foo bar baz"
strings = (stringPattern, stringSubject)

textPattern = Text.pack "foo (bar) baz"
textSubject = Text.pack "foo bar baz"

mkRegexBaseR :: String -> Text.Regex.PCRE.Regex
mkRegexBaseR patt = unsafePerformIO $ do
    Right r <- Text.Regex.PCRE.String.compile 0 0 patt
    return r

bgroupTexts :: String -> (String, String) -> Benchmark
bgroupTexts label (patt, subj) = bgroup label [
    bench "pcre2" $ flip nf textSubject $ \subj ->
        let Just (_ :| [bar]) = pcre2R subj in bar,

    bench "regex-pcre-builtin" $ nfIO $ do
        Text.Regex.PCRE.Text.regexec regexBaseR textSubject >>= \case
            Right (Just (_, _, _, [bar])) -> return bar
            x                             -> do
                print x
                error "BUG!",

    bench "pcre-light" $ flip nf textSubject $ \subj ->
        let Just [_, bar] = Text.Regex.PCRE.Light.Char8.match
                pcreLightR
                (Text.unpack subj)
                []
        in bar]

    where
    textPattern = Text.pack patt
    textSubject = Text.pack subj
    pcre2R = capturesA textPattern
    regexBaseR = mkRegexBaseR patt
    pcreLightR = Text.Regex.PCRE.Light.Char8.compile patt []
