{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Exception (evaluate)
import           Data.Functor      (void)
import           Data.IORef
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Test.Hspec
import           Text.Regex.Pcre2

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
        it "returns unset captures as empty strings" $ do
            let capturesWeirdness = captures "^(a)?b(c)?|d"
            capturesWeirdness "ab" `shouldBe` ["ab", "a", ""]
            capturesWeirdness "d"  `shouldBe` ["d",  "",  ""]

        -- verifies that NotEmpty makes a difference, see below
        it "matches an empty pattern to an empty subject" $ do
            match "" "" `shouldBe` Just ""

    describe "option handling" $ do
        it "handles compile options" $ do
            matchOpt Ungreedy "a+" "aaa" `shouldBe` Just "a"

        it "handles extra compile options" $ do
            matchOpt BadEscapeIsLiteral "\\j" "\\j" `shouldBe` Just "j"

        it "handles compile context options" $ do
            let otherBsr
                    | defaultBsr == BsrUnicode = BsrAnyCrlf
                    | otherwise                = BsrUnicode
                lineSep = "\x2028"
            matches "\\R" lineSep
                `shouldNotBe` matchesOpt (Bsr otherBsr) "\\R" lineSep

        -- We already know it handles compile recursion guards

        it "handles match options" $ do
            matchOpt NotEmpty "" "" `shouldBe` Nothing

        it "handles callouts" $ do
            calloutRanRef <- newIORef 0
            let callout = UnsafeCallout $ \_ -> do
                    modifyIORef calloutRanRef (+ 1)
                    return CalloutProceed
            matchOpt callout "(?C'foo')a(?C42)a" "aa" `shouldBe` Just "aa"
            readIORef calloutRanRef `shouldReturn` 2

        it "handles substitution callouts" $ do
            subCalloutRanRef <- newIORef 0
            let subCallout = UnsafeSubCallout $ \_ -> do
                    modifyIORef subCalloutRanRef (+ 1)
                    return SubCalloutAccept
            subOpt (subCallout <> SubGlobal) "a" "b" "aa" `shouldBe` "bb"
            readIORef subCalloutRanRef `shouldReturn` 2

        it "handles match context options" $ do
            matchOpt (OffsetLimit 10) "b" "aaaaaaaaaaab" `shouldBe` Nothing

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
