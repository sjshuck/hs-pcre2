-- | /Introduction/
--
-- Atop the low-level binding to the PCRE2 C API, we present a high-level
-- interface to add regular expression capabilities to Haskell applications and
-- libraries.
--
-- All inputs and outputs are strict `Text`, which maps directly to how our
-- included build of PCRE2 operates on strings of 16-bit-wide code units.
--
-- The C API requires pattern strings to be compiled and the compiled patterns
-- to be executed on subject strings in discrete steps.  We hide this
-- procedure, accepting pattern and subject as arguments in a single function.
-- The implementation guarantees that, when partially applied to pattern but not
-- subject, the resulting function will
-- [close](https://en.wikipedia.org/wiki/Closure_(computer_programming\))
-- on the underlying compiled pattern and reuse it for every subject it is
-- subsequently applied to.
-- 
-- Likewise, we do not require the user to know whether a PCRE2 option is to be
-- applied at pattern compile time or match time.  Instead we fold all possible
-- options into a single datatype, `Option`.  Most functions have vanilla and
-- configurable variants; the latter have @Opt@ in the name and accept an
-- @Option@.
--
-- Similar to how @head :: [a] -> a@ sacrifices totality for type simplicity,
-- we represent user errors (versus match failures) as imprecise exceptions.
-- Unlike with @head@, however, these exceptions are well-typed; moreover, we
-- offer Template Haskell facilities that can intercept some of these errors
-- at program compile time.
--
-- [There's more than one way to do it](https://en.wikipedia.org/wiki/There's_more_than_one_way_to_do_it)
-- with this library.  The choice between largely redundant presentations of
-- functionality&#x2014;functions versus @Traversal\'@s, type-indexed `Captures`
-- versus plain lists, string literals versus quasi-quotations, quasi-quoted
-- expressions versus quasi-quoted patterns&#x2014;is left to the user.  She will
-- observe that each advanced feature\'s additional safety, power, or
-- convenience entails additional language extensions, cognitive overhead, and
-- (for lenses) library dependencies.
--
-- /Definitions/
--
-- * __Pattern__.  The string defining a regular expression.  Refer to syntax
-- [here](https://pcre.org/current/doc/html/pcre2pattern.html).
--
-- * __Subject__.  The string the compiled regular expression is executed on.
--
-- * __Regex__.  A function of the form @`Text` -> result@, where the argument
-- is the subject.  (Lens users:  A regex has the more abstract form
-- @Traversal\' `Text` result@, but the concept is the same.)
--
-- * __Capture__ (or __capture group__).  Any substrings of the subject matched
-- by the pattern, meaning the whole pattern and any parenthesized groupings.
-- The PCRE2 docs do not refer to the former as a \"capture\"; however it is
-- accessed the same way in the C API, just with index 0, so we will consider it
-- the 0th capture for consistency.  Parenthesized captures are numbered from 1
-- in order of appearence of @(@.
--
-- * __Named capture__.  A parenthesized capture can be named @foo@ like this:
-- @(?\<foo\>...)@.  Whether they have names or not, captures are always
-- implicitly numbered as described above.
--
-- /Performance/
--
-- As discussed above, each API function is designed such that, when partially
-- applied to produce a regex, the underlying C data compiled from the pattern
-- and any options are reused for that regex\'s lifetime.  It is recommended to
-- create regexes as top-level values or else store them in a partially applied
-- state:
--
-- > serverGroups :: [(Text, Text -> Bool)]
-- > serverGroups = [
-- >     ("Hadoop",     matches "hadoop|hdp"),
-- >     ("Kubernetes", matches "kube|k8s"),
-- >     ("Unknown",    otherwise)]
--
-- This avoids forcing patterns to be recompiled via the C API for every match.
--
-- Note: Template Haskell regexes are immune from this problem and may be
-- freely inlined; see below.
module Text.Regex.Pcre2 (
    -- * Matching and substitution

    -- ** Basic matching functions
    match,
    matchOpt,
    matches,
    matchesOpt,
    captures,
    capturesOpt,
    capturesA,
    capturesOptA,

    -- ** PCRE2-native substitution
    sub,
    gsub,
    subOpt,

    -- ** Lens-powered matching and substitution
    --
    -- | To use this portion of the library, there are two prerequisites:
    --
    -- 1.  A basic working understanding of optics.  Many tutorials exist
    -- online, such as
    -- [this](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html),
    -- and videos such as [this](https://www.youtube.com/watch?v=7fbziKgQjnw).
    --
    -- 2.  A library providing combinators.  For lens newcomers, it is
    -- recommended to grab
    -- [microlens-platform](https://hackage.haskell.org/package/microlens-platform)&#x2014;all
    -- the examples in this library work with it, @packed@ and @unpacked@ are
    -- included for working with `Text`, and it is upwards-compatible with the
    -- full @lens@ library.
    --
    -- We expose a set of affine traversals that focus on matched substrings
    -- within a subject.  Like the basic functional regexes, they should be
    -- \"compiled\" and memoized, rather than created inline.
    --
    -- > _nee :: Traversal' Text Text
    -- > _nee = _match "(?i)\\bnee\\b"
    --
    -- In addition to getting results, they support substitution through
    -- setting; more generally, they can accrete effects while performing
    -- replacements generated on the fly.
    --
    -- >>> promptNee = traverseOf (_nee . unpacked) $ \s -> print s >> getLine
    -- >>> promptNee "We are the knights who say...NEE!"
    -- "NEE"
    -- NOO
    -- "We are the knights who say...NOO!"
    -- >>>
    --
    -- The optic interface signals failure by not targeting anything.
    --
    -- >>> promptNee "Shhhhh"
    -- "Shhhhh"
    -- >>>
    --
    -- In general these @Traversal'@s are not law-abiding.
    _match,
    _matchOpt,
    _captures,
    _capturesOpt,

    -- * Compile-time validation
    --
    -- | Despite whatever virtues, the API thus far has some fragility arising
    -- from various scenarios:
    --
    -- * malformed patterns such as mismatched parentheses /(runtime error)/
    --
    -- * out-of-bounds indexing of a capture group list /(runtime error)/
    --  
    -- * out-of-bounds @ix@ing of a @Traversal\'@ target
    -- /(spurious failure to match)/
    --
    -- * a case expression containing a Haskell list pattern of the wrong length
    -- /(spurious failure to match)/
    --
    -- * a regex inadvertently created inline /(suboptimal performance)/
    --
    -- * ugly\/incorrect number of backslashes in a pattern&#x2014;matching
    -- a literal backslash requires the pattern string @\"\\\\\\\\\"@
    -- /(difficulty maintaining\/debugging)/
    --
    -- Using a combination of GHC language extensions and PCRE2 pattern
    -- introspection features, we provide a Template Haskell API to mitigate or
    -- prevent all these scenarios.

    -- ** Quasi-quoters
    regex,
    _regex,

    -- ** Type-indexed capture groups
    Captures(),
    capture,
    _capture,

    -- * Options

    Option(
        AllowEmptyClass,
        AltBsux,
        AltBsuxLegacy,
        AltCircumflex,
        AltVerbNames,
        Anchored,
        AutoCallout,
        BadEscapeIsLiteral,
        Bsr,
        Caseless,
        DepthLimit,
        DollarEndOnly,
        DotAll,
        DupNames,
        EndAnchored,
        EscapedCrIsLf,
        Extended,
        ExtendedMore,
        FirstLine,
        HeapLimit,
        Literal,
        MatchLimit,
        MatchLine,
        MatchUnsetBackRef,
        MatchWord,
        MaxPatternLength,
        Multiline,
        NeverBackslashC,
        NeverUcp,
        Newline,
        NoAutoCapture,
        NoAutoPossess,
        NoDotStarAnchor,
        NoStartOptimize,
        NotBol,
        NotEmpty,
        NotEmptyAtStart,
        NotEol,
        OffsetLimit,
        ParensLimit,
        PartialHard,
        PartialSoft,
        SubGlobal,
        SubLiteral,
        SubReplacementOnly,
        SubUnknownEmpty,
        Ucp,
        Ungreedy,
        UnsafeCallout,
        UnsafeCompileRecGuard,
        UnsafeSubCallout,
        Utf),

    Bsr(..),
    Newline(..),

    -- ** Callout interface
    -- *** Capture callouts
    CalloutInfo(..),
    CalloutIndex(..),
    CalloutResult(..),
    -- *** Substitution callouts
    SubCalloutInfo(..),
    SubCalloutResult(..),

    -- * Dealing with exceptions
    SomePcre2Exception(),
    Pcre2Exception(),
    Pcre2CompileException(),

    -- * PCRE2 build configuration
    defaultBsr,
    compiledWidths,
    defaultDepthLimit,
    defaultHeapLimit,
    supportsJit,
    jitTarget,
    linkSize,
    defaultMatchLimit,
    defaultNewline,
    defaultIsNeverBackslashC,
    defaultParensLimit,
    defaultTablesLength,
    unicodeVersion,
    supportsUnicode,
    pcreVersion)
where

import Data.Text                 (Text)
import Text.Regex.Pcre2.Internal
import Text.Regex.Pcre2.TH
