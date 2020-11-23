{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Exception   (evaluate, handle)
import           Data.Functor        (void)
import           Data.IORef
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Lens.Micro.Platform
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
        -- verifies that NotEmpty makes a difference, see below
        it "matches an empty pattern to an empty subject" $ do
            match "" "" `shouldBe` Just ""

        it "treats an unset capture as empty" $ do
            captures "(a)?" "" `shouldBe` ["", ""]

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

    describe "user error handling" $ do
        let handleMatch = handle @SomePcre2Exception $ \_ -> return "handled"

        it "can be accomplished using Control.Exception.evaluate" $ do
            handleMatch (evaluate (match @Maybe "*" "") >> return "not handled")
                `shouldReturn` "handled"

        it "can be accomplished using instance Alternative IO" $ do
            handleMatch (match "*" "") `shouldReturn` "handled"

    describe "an unset capture" $ do
        it "is unchanged via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @1) "foo" "" `shouldBe` ""

        it "permits other captures to be changed via Traversal'" $ do
            set ([_regex|(a)?|] . _capture @0) "foo" "" `shouldBe` "foo"

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
