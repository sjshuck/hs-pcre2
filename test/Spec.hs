{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Applicative     (Alternative)
import           Control.Exception
import           Control.Monad           (forM_, void)
import           Control.Monad.RWS.Lazy  (RWS, ask, evalRWS, tell)
import           Data.IORef              (modifyIORef', newIORef, readIORef)
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Lens.Micro.Platform
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Text.Printf             (printf)
import           Text.Regex.Pcre2
import           Text.Regex.Pcre2.Unsafe

main :: IO ()
main = defaultMain $ testGroup "tests" [
    testGroup "partial application" [
        testCase "only causes one compilation" $ do
            onlyCausesOneCompilation $ \option ->
                matchesOpt option "foo",

        testCase "only causes one compilation (point-free expression)" $ do
            onlyCausesOneCompilation $ \option ->
                (||) <$> Text.null <*> matchesOpt option "foo"],

    testGroup "basic matching" [
        -- verifies that NotEmpty makes a difference, see below
        testCase "matches an empty pattern to an empty subject" $ do
            match "" "" @?= Just "",

        testCase "works using matches" $ do
            matches "(?i)foo" "FOO" @?= True,

        testCase "works using captures" $ do
            case captures "(\\d{4})-(\\d{2})-(\\d{2})" submitted of
                Just ne -> ne @?= "2020-10-20" :| ["2020", "10", "20"]
                Nothing -> assertFailure "didn't match",

        testCase "is lazy" $ do
            counter <- newIORef (0 :: Int)
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef' counter (+ 1)
                    return CalloutProceed
            take 3 (matchOpt callout "(?C1)a" "apples and bananas")
                @?= ["a", "a", "a"]
            readIORef counter >>= (@?= 3),

        testCase ("fills up Alternative containers" `issue` 18) $ do
            let result :: (Alternative f) => f Text
                result = match "\\d+" "123 456"
            result @?= Just "123"
            result @?= ["123", "456"],

        testCase ("is in UTF mode" `issue` 26) $ do
            matchesOpt (Bsr BsrUnicode) "\\R$" "line separator: \x2028"
                @?= True,

        testCase ("doesn't use-after-free() for long matches" `issue` 39) $ do
            let subj = Text.replicate 20000 "a"
            length (captures @[] "." subj) @?= 20000],

    testGroup "lens-powered matching" [
        testCase "can accrete effects while performing replacements" $ do
            let result = evalRWS
                    (promptNee "We are the knights who say...NEE!")
                    "NOO"
                    ()
            result @?= ("We are the knights who say...NOO!", ["NEE"]),

        testCase "signals match failure by not targeting anything" $ do
            let result = evalRWS
                    (promptNee "Shhhhh")
                    (error "should be unreachable")
                    ()
            result @?= ("Shhhhh", []),

        testCase "does not substitute when setting equal Text" $ do
            let threeAndMiddle = _captures ". (.) ."
            ("A A A" & threeAndMiddle .~ "A A A" :| ["B"]) @?= "A B A"
            ("A A A" & threeAndMiddle .~ "A B A" :| ["A"]) @?= "A B A"],

    testGroup "option handling" [
        testCase "includes compile options" $ do
            matchOpt Ungreedy "a+" "aaa" @?= Just "a",

        testCase "includes extra compile options" $ do
            matchOpt BadEscapeIsLiteral "\\j" "\\j" @?= Just "j",

        testCase "includes compile context options" $ do
            let bsrMatchesFF bsr = matchesOpt (Bsr bsr) "\\R" "\f"
            bsrMatchesFF BsrUnicode @?= True
            bsrMatchesFF BsrAnyCrlf @?= False,

        -- We already know it includes compile recursion guards

        testCase "includes match options" $ do
            matchOpt NotEmpty "" "" @?= Nothing,

        testCase "includes callouts" $ do
            calloutRanRef <- newIORef 0
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef' calloutRanRef (+ 1)
                    return CalloutProceed
            matchOpt callout "(?C'foo')a(?C42)a" "aa" @?= Just "aa"
            readIORef calloutRanRef >>= (@?= 2),

        testCase "includes substitution callouts" $ do
            subCalloutRanRef <- newIORef 0
            let subCallout = UnsafeSubCallout $ \_ -> do
                    modifyIORef' subCalloutRanRef (+ 1)
                    return SubCalloutAccept
            subOpt (subCallout <> SubGlobal) "a" "b" "aa" @?= "bb"
            readIORef subCalloutRanRef >>= (@?= 2),

        testCase "includes match context options" $ do
            matchOpt (OffsetLimit 10) "b" "aaaaaaaaaaab" @?= Nothing],

    testGroup "user errors" [
        testCase "are thrown at match time" $ do
            assertThrow @SomePcre2Exception $
                broken @Maybe "foo" `seq` return (),

        testCase "are catchable using Control.Exception.evaluate" $ do
            let example = handle @SomePcre2Exception (\_ -> return Nothing) $
                    evaluate $ broken "foo"
            example >>= (@?= Nothing),

        testCase "are catchable using instance Alternative IO" $ do
            let example = handle @SomePcre2Exception (\_ -> return "broken") $
                    broken "foo"
            example >>= (@?= "broken")],

    testGroup "native substitution" [
        testCase "works using sub" $ do
            let result = sub
                    "(\\w+) calling the (\\w+)"
                    "$2 calling the $1"
                    "the pot calling the kettle black"
            result @?= "the kettle calling the pot black",

        testCase "works using gsub" $ do
            gsub "a" "o" "apples and bananas" @?= "opples ond bononos"],

    testGroup "regex :: QuasiQuoter" [
        testGroup "as an expression" [
            testCase "works with parenthesized captures" $ do
                case [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] submitted of
                    Nothing -> assertFailure "regex didn't match"
                    Just cs -> do
                        let date = capture @0 cs
                            year = read @Int $ Text.unpack $ capture @"y" cs
                        (date, year) @?= ("2020-10-20", 2020),

            testCase "works without parenthesized captures" $ do
                let example = forM_ @Maybe ([regex|^\s+$|] "  ") $ \spaces ->
                        error $ "line has spaces only: " ++ show spaces
                assertThrow @ErrorCall example],

        testGroup "as a pattern" [
            testCase "works with named captures" $ do
                case submitted of
                    [regex|(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})|] -> do
                        let year = read @Int $ Text.unpack y
                        year @?= 2020
                    _ -> assertFailure "regex didn't match",

            testCase ("works with possibly unset named captures" `issue` 4) $ do
                let f = fmap (capture @"b") . [regex|(?<a>a)|(?<b>b)|]
                f "a" >>= (@?= "")
                f "b" >>= (@?= "b"),

            testCase "works without named captures" $ do
                case "abc" of
                    [regex|a(b)c|] -> return ()
                    _              -> assertFailure "regex didn't match"]],

    testGroup "_regex" [
        testCase "works with parenthesized captures" $ do
            -- Adapt the _captures example for use here
            let result = "Well bust my buttons!" &~ do
                    zoom [_regex|(\w+) my (\w+)|] $ do
                        _capture @1 . _head .= 'd'
                        _capture @2 %= Text.reverse
                    _last .= '?'
            result @?= "Well dust my snottub?",

        testCase "works without parenthesized captures" $ do
            let embeddedNumber :: Traversal' String Int
                embeddedNumber = packed . [_regex|\d+|] . unpacked . _Show
                result = "There are 14 competing standards"
                    & embeddedNumber %~ (+ 1)
            result @?= "There are 15 competing standards"],

    testGroup "Captures" [

        testCase "has a predictable Show instance" $ do
            cs <- mkCaptures
            show cs @?= "Captures (\"abc\" :| [\"b\",\"c\"])",

        testCase "can have its underlying list extracted" $ do
            cs <- mkCaptures
            getCaptures cs @?= "abc" :| ["b", "c"]],

    testGroup "predictCapturesInfo" [
        testCase ("analyzes captures groups' names" `issue` 45) $ do
            info <- predictCapturesInfo mempty
                "foo (?<bar>...) (?<a>[[:alpha:]]) (ba*z)"
            info @?= (3, [("bar", 1), ("a", 2)])],

    testGroup "an unset capture" [
        testCase "is treated as empty" $ do
            captures "(a)?" "" @?= Just ("" :| [""]),

        testCase "is unchanged via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @1) "foo" "" @?= "",

        testCase "permits other captures to be changed via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @0) "foo" "" @?= "foo"],

    testGroup "Traversal'" [
        testCase "supports global substitutions" $ do
            set (_match "a") "o" "apples and bananas" @?= "opples ond bononos",

        testCase "converges in the presence of empty matches" $ do
            length (match @[] "" "12345") @?= 6],

    testGroup "PCRE2 build configuration" [
        testCase ("includes Unicode support" `issue` 21) $ do
            matchesOpt Ucp "\\w$" "aleph: \x2135" @?= True]]

-- | Modify label of `testGroup`, `testCase`, etc. to include a link to a Github
-- issue.
issue :: String -> Int -> String
issue = printf "%s (https://github.com/sjshuck/hs-pcre2/issues/%d)"

onlyCausesOneCompilation :: (Option -> Text -> a) -> Assertion
onlyCausesOneCompilation regexWithOpt = do
    counter <- newIORef (0 :: Int)
    let option = UnsafeCompileRecGuard $ \_ -> do
            modifyIORef' counter (+ 1)
            return True
        run = void . evaluate . regexWithOpt option
        getCount = readIORef counter

    run "foo"
    countAfterOnce <- getCount
    countAfterOnce > 0  -- probably 2!
        @? "compile recursion guard didn't run after first match"

    run "bar"
    run "baz"
    countAfterTwiceMore <- getCount
    countAfterTwiceMore == countAfterOnce
        @? "compile recursion guard ran again"

broken :: (Alternative f) => Text -> f Text
broken = match "*"

submitted :: Text
submitted = "submitted 2020-10-20"

promptNee :: Text -> RWS String [String] () Text
promptNee = traverseOf (_nee . unpacked) $ \s -> tell [s] >> ask where
    _nee = _matchOpt (Caseless <> MatchWord) "nee"

mkCaptures :: IO (Captures '(2, '[ '("c", 2)]))
mkCaptures = [regex|a(b)(?<c>c)|] "abc"

assertThrow :: forall e a. (Exception e) => IO a -> Assertion
assertThrow action =
    try @e action >>= traverseOf_ _Right (\_ -> assertFailure "didn't throw")
