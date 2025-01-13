-- | These items are unsafe for one reason or another, and are sequestered here
-- to require the user to do an extra import to get them.
--
-- Chief among
-- them is the callout interface: these options and associated datatypes may be
-- used to register effectful callbacks, sometimes referred to as /callouts/ in
-- the PCRE2 API, for regex compilation, matching, and substitution.  We include
-- them here for completeness and use them to implement unit tests for this
-- library; for ordinary use, however, seek other means to accomplish whatever
-- is needed (such as accreting effects with optics), since they carry all the
-- problems of `System.IO.Unsafe.unsafePerformIO`.  See
-- the [C API docs](https://pcre.org/current/doc/html/pcre2callout.html)
-- for more information.
module Text.Regex.Pcre2.Unsafe (
    -- ** Options
    Option(
        AutoCallout,
        BadEscapeIsLiteral,
        UnsafeCallout,
        UnsafeCompileRecGuard,
        UnsafeSubCallout),

    -- ** Types
    CalloutInfo(..),
    CalloutIndex(..),
    CalloutResult(..),
    SubCalloutInfo(..),
    SubCalloutResult(..))
where

import Text.Regex.Pcre2.Internal
