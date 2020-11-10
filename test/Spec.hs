{-# LANGUAGE DataKinds #-}         -- exp
{-# LANGUAGE OverloadedStrings #-} -- exp & pat
{-# LANGUAGE QuasiQuotes #-}       -- exp & pat
{-# LANGUAGE TemplateHaskell #-}   -- exp & pat
{-# LANGUAGE TypeApplications #-}  -- exp
{-# LANGUAGE TypeOperators #-}     -- exp
{-# LANGUAGE ViewPatterns #-}      --       pat

module Main where

import           Data.Function             ((&))
import           Data.Functor.Const        (Const(..))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Text.Regex.Pcre2.Internal
import           Text.Regex.Pcre2.TH

(^.) = flip view
infixl 8 ^.

(%~) = over
infixr 4 %~

l .~ x = l %~ const x
infixr 4 .~
set = (.~)

main :: IO ()
main = do
    putStrLn $ "Tables length: " ++ show defaultTablesLength

    putStrLn $ "JIT support: " ++ show supportsJit

    case "foo bar baz" of
        [re|\s+(?<middle>\S+)|] -> print middle
        subject                 -> error $ show subject ++ " doesn't match"

    case "shump" of
        [re|hum|] -> print True
        _         -> print False

    case [re|\s+(?<middle>\S+)|] "foo bar baz" of
        Just cs -> print (capture @"middle" cs) >> print (capture @1 cs)
        Nothing -> error "Doesn't match"

    print $ "foo bar baz"
        & [_re|\s+(?<middle>\S+)|] . _capture @"middle" %~ Text.reverse

    print $ "alter-cocker"
        & [_re|\s+(?<middle>\S+)|] . _capture @"middle" %~ Text.reverse

    print $ "The pot calling the kettle black"
        & [_re|[Tt]he (\S+).*?the (\S+)|] %~ \cs -> cs
            & _capture @1 .~ cs ^. _capture @2
            & _capture @2 .~ cs ^. _capture @1

    print $ "The pot calling the kettle black"
        & [_re|[Tt]he (\S+).*?the (\S+)|] %~ do
            c1 <- capture @1
            c2 <- capture @2
            set (_capture @2) c1 . set (_capture @1) c2

    let flippy = sub "a" "o"
    print $ flippy "apples and bananas"

    let myOpts = mconcat [
            SubGlobal,
            UnsafeCallout $ \info -> do
                print info
                return CalloutProceed,
            UnsafeSubCallout $ \info -> do
                print info
                if subCalloutSubsCount info `div` 2 == 0
                    then return SubCalloutSkip
                    else return SubCalloutAccept,
            UnsafeCompileRecGuard $ \i -> print i >> return True]
        flippier = subOpt myOpts "(?C1)(a)(?C'xyz')" "o-------------"

    print $ flippier "apples and bananas"
