{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Applicative     (Alternative)
import           Control.Exception       (catch, evaluate, handle)
import           Control.Monad           (forM_, void)
import           Control.Monad.RWS.Lazy  (ask, evalRWS, tell)
import           Data.IORef              (modifyIORef', newIORef, readIORef)
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Lens.Micro.Platform
import           Test.Hspec
import           Text.Printf             (printf)
import           Text.Regex.Pcre2
import           Text.Regex.Pcre2.Unsafe

main :: IO ()
main = hspec $ do
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

        it "works using captures" $ do
            case captures "(\\d{4})-(\\d{2})-(\\d{2})" submitted of
                Just ne -> ne `shouldBe` "2020-10-20" :| ["2020", "10", "20"]
                Nothing -> expectationFailure "didn't match"

        it "is lazy" $ do
            counter <- newIORef (0 :: Int)
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef' counter (+ 1)
                    return CalloutProceed
            take 3 (matchOpt callout "(?C1)a" "apples and bananas")
                `shouldBe` ["a", "a", "a"]
            readIORef counter `shouldReturn` 3

        it ("fills up Alternative containers" `issue` 18) $ do
            let result :: (Alternative f) => f Text
                result = match "\\d+" "123 456"
            result `shouldBe` Just "123"
            result `shouldBe` ["123", "456"]

    describe "lens-powered matching" $ do
        let _nee :: Traversal' Text Text
            _nee = _matchOpt (Caseless <> MatchWord) "nee"
            promptNee = traverseOf (_nee . unpacked) $ \s -> tell [s] >> ask

        it "can accrete effects while performing replacements" $ do
            let result = evalRWS
                    (promptNee "We are the knights who say...NEE!")
                    "NOO"
                    ()
            result `shouldBe` ("We are the knights who say...NOO!", ["NEE"])

        it "signals match failure by not targeting anything" $ do
            let result = evalRWS
                    (promptNee "Shhhhh")
                    (error "should be unreachable")
                    ()
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
            let bsrMatchesFF bsr = matchesOpt (Bsr bsr) "\\R" "\f"
            bsrMatchesFF BsrUnicode `shouldBe` True
            bsrMatchesFF BsrAnyCrlf `shouldBe` False

        -- We already know it includes compile recursion guards

        it "includes match options" $ do
            matchOpt NotEmpty "" "" `shouldBe` Nothing

        it "includes callouts" $ do
            calloutRanRef <- newIORef 0
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef' calloutRanRef (+ 1)
                    return CalloutProceed
            matchOpt callout "(?C'foo')a(?C42)a" "aa" `shouldBe` Just "aa"
            readIORef calloutRanRef `shouldReturn` 2

        it "includes substitution callouts" $ do
            subCalloutRanRef <- newIORef 0
            let subCallout = UnsafeSubCallout $ \_ -> do
                    modifyIORef' subCalloutRanRef (+ 1)
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
            let example = handle @SomePcre2Exception (\_ -> return Nothing) $
                    evaluate $ broken "foo"
            example `shouldReturn` Nothing

        it "are catchable using instance Alternative IO" $ do
            let example = handle @SomePcre2Exception (\_ -> return "broken") $
                    broken "foo"
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

            it ("works with possibly unset named captures" `issue` 4) $ do
                let f = fmap (capture @"b") . [regex|(?<a>a)|(?<b>b)|]
                f "a" `shouldReturn` ""
                f "b" `shouldReturn` "b"

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

    describe "Captures" $ do
        it "have a predictable Show instance" $ do
            cs <- [regex|a(b)(?<c>c)|] "abc"
            show cs `shouldBe` "Captures (\"abc\" :| [\"b\",\"c\"])"

    describe "an unset capture" $ do
        it "is treated as empty" $ do
            captures "(a)?" "" `shouldBe` Just ("" :| [""])

        it "is unchanged via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @1) "foo" "" `shouldBe` ""

        it "permits other captures to be changed via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @0) "foo" "" `shouldBe` "foo"

    describe "Traversal'" $ do
        it "supports global substitutions" $ do
            set (_match "a") "o" "apples and bananas"
                `shouldBe` "opples ond bononos"

        it "converges in the presence of empty matches" $ do
            length (match @[] "" "12345") `shouldBe` 6

    describe "PCRE2 build configuration" $ do
        it ("includes Unicode support" `issue` 21) $ do
            matchesOpt Ucp "\\w$" "aleph: \x2135" `shouldBe` True

-- | Modify label of `describe`, `it`, etc. to include a link to a Github issue.
issue :: String -> Int -> String
issue = printf "%s (https://github.com/sjshuck/hs-pcre2/issues/%d)"

onlyCausesOneCompilation :: (Option -> Text -> a) -> Expectation
onlyCausesOneCompilation regexWithOpt = do
    counter <- newIORef (0 :: Int)
    let option = UnsafeCompileRecGuard $ \_ -> do
            modifyIORef' counter (+ 1)
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

submitted :: Text
submitted = "submitted 2020-10-20"

broken :: (Alternative f) => Text -> f Text
broken = match "*"
