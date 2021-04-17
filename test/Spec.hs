{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Applicative    (Alternative)
import           Control.Exception      (catch, evaluate, handle)
import           Control.Monad.RWS.Lazy (ask, evalRWS, forM_, tell, void)
import           Data.IORef
import           Data.List.NonEmpty     (NonEmpty(..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Lens.Micro.Platform
import           Test.Hspec
import           Text.Regex.Pcre2

main :: IO ()
main = hspec $ do
    let submitted = "submitted 2020-10-20"

    describe "partial application" $ do
        it "only causes one compilation" $ do
            onlyCausesOneCompilation $ \option ->
                matchesOpt option "foo"

        it "only causes one compilation (point-free expression)" $ do
            onlyCausesOneCompilation $ \option ->
                (||) <$> Text.null <*> matchesOpt option "foo"

    describe "basic matching" $ do
        -- verifies that NotEmpty makes a difference, see below
        it "matches an empty pattern to an empty subject" $ do
            match "" "" `shouldBe` Just ""

        it "works using matches" $ do
            matches "(?i)foo" "FOO" `shouldBe` True

        it "works using capturesA" $ do
            case capturesA "(\\d{4})-(\\d{2})-(\\d{2})" submitted of
                Just ne -> ne `shouldBe` "2020-10-20" :| ["2020", "10", "20"]
                Nothing -> expectationFailure "didn't match"

    describe "lens-powered matching" $ do
        let _nee :: Traversal' Text Text
            _nee = _match "(?i)\\bnee\\b"
            promptNee = traverseOf (_nee . unpacked) $ \s -> tell [s] >> ask

        it "can accrete effects while performing replacements" $ do
            let result = evalRWS
                    (promptNee "We are the knights who say...NEE!")
                    "NOO"
                    undefined
            result `shouldBe` ("We are the knights who say...NOO!", ["NEE"])

        it "signals match failure by not targeting anything" $ do
            let result = evalRWS
                    (promptNee "Shhhhh")
                    undefined
                    undefined
            result `shouldBe` ("Shhhhh", [])

        it "does not substitute when setting equal Text" $ do
            let threeAndMiddle = _captures ". (.) ."
            ("A A A" & threeAndMiddle .~ "A A A" :| ["B"]) `shouldBe` "A B A"
            ("A A A" & threeAndMiddle .~ "A B A" :| ["A"]) `shouldBe` "A B A"

    describe "option handling" $ do
        it "includes compile options" $ do
            matchOpt Ungreedy "a+" "aaa" `shouldBe` Just "a"

        it "includes extra compile options" $ do
            matchOpt BadEscapeIsLiteral "\\j" "\\j" `shouldBe` Just "j"

        it "includes compile context options" $ do
            let otherBsr
                    | defaultBsr == BsrUnicode = BsrAnyCrlf
                    | otherwise                = BsrUnicode
                lineSep = "\x2028"
            matches "\\R" lineSep
                `shouldNotBe` matchesOpt (Bsr otherBsr) "\\R" lineSep

        -- We already know it includes compile recursion guards

        it "includes match options" $ do
            matchOpt NotEmpty "" "" `shouldBe` Nothing

        it "includes callouts" $ do
            calloutRanRef <- newIORef 0
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef calloutRanRef (+ 1)
                    return CalloutProceed
            matchOpt callout "(?C'foo')a(?C42)a" "aa" `shouldBe` Just "aa"
            readIORef calloutRanRef `shouldReturn` 2

        it "includes substitution callouts" $ do
            subCalloutRanRef <- newIORef 0
            let subCallout = UnsafeSubCallout $ \_ -> do
                    modifyIORef subCalloutRanRef (+ 1)
                    return SubCalloutAccept
            subOpt (subCallout <> SubGlobal) "a" "b" "aa" `shouldBe` "bb"
            readIORef subCalloutRanRef `shouldReturn` 2

        it "includes match context options" $ do
            matchOpt (OffsetLimit 10) "b" "aaaaaaaaaaab" `shouldBe` Nothing

    describe "user errors" $ do
        it "are thrown at match time" $ do
            (broken @Maybe "foo" `seq` return ())
                `shouldThrow` anyPcre2Exception

        it "are catchable using Control.Exception.evaluate" $ do
            let example = evaluate (broken "foo") `catch`
                    \(_ :: SomePcre2Exception) -> return Nothing
            example `shouldReturn` Nothing

        it "are catchable using instance Alternative IO" $ do
            let example = broken "foo" `catch`
                    \(_ :: SomePcre2Exception) -> return "broken"
            example `shouldReturn` "broken"

    describe "native substitution" $ do
        it "works using sub" $ do
            let result = sub
                    "(\\w+) calling the (\\w+)"
                    "$2 calling the $1"
                    "the pot calling the kettle black"
            result `shouldBe` "the kettle calling the pot black"

        it "works using gsub" $ do
            gsub "a" "o" "apples and bananas" `shouldBe` "opples ond bononos"

    describe "regex :: QuasiQuoter" $ do
        context "as an expression" $ do
            it "works with parenthesized captures" $ do
                case [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] submitted of
                    Nothing -> expectationFailure "regex didn't match"
                    Just cs -> do
                        let date = capture @0 cs
                            year = read @Int $ Text.unpack $ capture @"y" cs
                        (date, year) `shouldBe` ("2020-10-20", 2020)

            it "works without parenthesized captures" $ do
                let example = forM_ @Maybe ([regex|^\s+$|] "  ") $ \spaces ->
                        error $ "line has spaces only: " ++ show spaces
                example `shouldThrow` anyErrorCall

        context "as a pattern" $ do
            it "works with named captures" $ do
                case submitted of
                    [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] -> do
                        let year = read @Int $ Text.unpack y
                        year `shouldBe` 2020
                    _ -> expectationFailure "regex didn't match"

            it "works without named captures" $ do
                case "abc" of
                    [regex|a(b)c|] -> return ()
                    _              -> expectationFailure "regex didn't match"

    describe "_regex" $ do
        it "works with parenthesized captures" $ do
            -- Adapt the _captures example for use here
            let result = "Well bust my buttons!" &~ do
                    zoom [_regex|(\w+) my (\w+)|] $ do
                        _capture @1 . _head .= 'd'
                        _capture @2 %= Text.reverse
                    _last .= '?'
            result `shouldBe` "Well dust my snottub?"

        it "works without parenthesized captures" $ do
            let embeddedNumber :: Traversal' String Int
                embeddedNumber = packed . [_regex|\d+|] . unpacked . _Show
                result = "There are 14 competing standards"
                    & embeddedNumber %~ (+ 1)
            result `shouldBe` "There are 15 competing standards"

    describe "an unset capture" $ do
        it "is treated as empty" $ do
            captures "(a)?" "" `shouldBe` ["", ""]

        it "is unchanged via Traversal'" $ do
            ("" & [_regex|(a)?|] . _capture @1 .~ "foo") `shouldBe` ""

        it "permits other captures to be changed via Traversal'" $ do
            ("" & [_regex|(a)?|] . _capture @0 .~ "foo") `shouldBe` "foo"

    describe "Traversal'" $ do
        it "supports global substitutions" $ do
            ("apples and bananas" & _match "a" .~ "o")
                `shouldBe` "opples ond bononos"

        it "is lazy" $ do
            counter <- newIORef (0 :: Int)
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef counter (+ 1)
                    return CalloutProceed
            take 3 ("apples and bananas" ^.. _matchOpt callout "(?C1)a")
                `shouldBe` ["a", "a", "a"]
            readIORef counter `shouldReturn` 3

        it "converges in the presence of empty matches" $ do
            length (toListOf [_regex||] "12345") `shouldBe` 6

    describe "bug fixes" bugFixes

bugFixes :: Spec
bugFixes = do
    issue 4 $ do
        let f = fmap (capture @"b") . [regex|(?<a>a)|(?<b>b)|]
        f "a" `shouldReturn` ""
        f "b" `shouldReturn` "b"

    issue 12 $ do
        captures ".a." "foo bar baz" `shouldBe` ["bar"]
        case "foo bar baz" of
            [regex|.a(?<x>.)|] -> return ()
            _                  ->
                expectationFailure "quasi-quoted pattern didn't match"

    where
    issue :: Int -> Expectation -> Spec
    issue n = it $ "https://github.com/sjshuck/hs-pcre2/issues/" ++ show n

onlyCausesOneCompilation :: (Option -> Text -> a) -> Expectation
onlyCausesOneCompilation regexWithOpt = do
    counter <- newIORef (0 :: Int)
    let option = UnsafeCompileRecGuard $ \_ -> do
            modifyIORef counter (+ 1)
            return True
        run = void . evaluate . regexWithOpt option
        getCount = readIORef counter

    run "foo"
    countAfterOnce <- getCount
    countAfterOnce `shouldSatisfy` (> 0) -- probably 2!

    run "bar"
    run "baz"
    countAfterTwiceMore <- getCount
    countAfterTwiceMore `shouldBe` countAfterOnce

anyPcre2Exception :: Selector SomePcre2Exception
anyPcre2Exception _ = True

broken :: (Alternative f) => Text -> f Text
broken = match "*"

-- microlens doesn't have this yet as of 12/5/2020
_Show :: (Read a, Show a) => Traversal' String a
_Show f s = case reads s of
    [(x, "")] -> show <$> f x
    _         -> pure s
