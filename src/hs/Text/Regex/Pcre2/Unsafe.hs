{-# OPTIONS_HADDOCK not-home #-}

module Text.Regex.Pcre2.Unsafe (
    -- * The callout interface
    --
    -- | These options and associated datatypes may be used to register
    -- effectful callbacks, sometimes referred to as _callouts_ in the PCRE2
    -- API, for regex compilation, matching, and substitution.  We include them
    -- here for completeness and use them to implement unit tests for this
    -- library; for ordinary use, however, see if other means can accomplish
    -- whatever is needed (such as accreting effects with optics), since they
    -- carry all the problems of `unsafePerformIO`.
    --
    -- See the [C API docs](https://pcre.org/current/doc/html/pcre2callout.html)
    -- for more information.

    Option(
        UnsafeCompileRecGuard,
        AutoCallout,
        UnsafeCallout,
        UnsafeSubCallout),

    -- ** Types
    CalloutInfo(..),
    CalloutIndex(..),
    CalloutResult(..),
    SubCalloutInfo(..),
    SubCalloutResult(..))
where

import System.IO.Unsafe          (unsafePerformIO)
import Text.Regex.Pcre2.Internal
