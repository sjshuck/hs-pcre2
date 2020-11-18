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
        it "only causes one compilation" $
            onlyCausesOneCompilation $ \option ->
                matchesOpt option "foo"

        it "only causes one compilation (point-free expression)" $ do
            onlyCausesOneCompilation $ \option ->
                (||) <$> Text.null <*> matchesOpt option "foo"
    
    describe "basic matching functions" $ do
        it "is a start" $
            capturesOpt Caseless "a(b)?(c)" "ac"
                `shouldBe` ["ac", "c"]

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
