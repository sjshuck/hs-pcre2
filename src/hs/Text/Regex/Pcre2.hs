-- | __Introduction__
--
-- Atop the low-level binding to the C API, we present a high-level interface to
-- add regular expressions to Haskell programs.
--
-- All input and output strings are strict `Text`, which maps directly to how
-- PCRE2 operates on strings of 16-bit-wide code units.
--
-- The C API requires pattern strings to be compiled and the compiled patterns
-- to be executed on subject strings in discrete steps.  We hide this
-- procedure, accepting pattern and subject as arguments in a single function:
--
-- > pattern -> subject -> result
--
-- The implementation guarantees that, when partially applied to pattern but not
-- subject, the resulting function will
-- [close](https://en.wikipedia.org/wiki/Closure_(computer_programming\))
-- on the underlying compiled object and reuse it for every subject it is
-- subsequently applied to.
-- 
-- Likewise, we do not require the user to know whether a PCRE2 option is to be
-- applied at pattern compile time or match time.  Instead we fold all possible
-- options into a single datatype, `Option`.  Most functions have vanilla and
-- configurable variants; the latter have \"@Opt@\" in the name and accept a
-- value of this type.
--
-- Similar to how @head :: [a] -> a@ sacrifices totality for type simplicity,
-- we represent user errors as imprecise exceptions.  Unlike with @head@, these
-- exceptions are typed (as `SomePcre2Exception`s); moreover, we offer Template
-- Haskell facilities that can intercept some of these errors before the program
-- is run.  (Failure to match is not considered a user error and is represented
-- in the types.)
--
-- [There's more than one way to do it](https://en.wikipedia.org/wiki/There's_more_than_one_way_to_do_it)
-- with this library.  The choices between functions and @Traversal\'@s,
-- poly-kinded `Captures` and plain lists, string literals and quasi-quotations,
-- quasi-quoted expressions and quasi-quoted patterns....These are left to the
-- user.  She will observe that advanced features\' extra safety, power, and
-- convenience entail additional language extensions, cognitive overhead, and
-- (for lenses) library dependencies, so it\'s really a matter of finding the
-- best trade-offs for her case.
--
-- __Definitions__
--
-- * /Pattern/.  The string defining a regular expression.  Refer to syntax
-- [here](https://pcre.org/current/doc/html/pcre2pattern.html).
--
-- * /Subject/.  The string the compiled regular expression is executed on.
--
-- * /Regex/.  A function of the form @`Text` -> result@, where the argument
-- is the subject.  It is \"compiled\" via partial application as discussed
-- above.  (Lens users:  A regex has the more abstract form @Traversal\' `Text`
-- result@, but the concept is the same.)
--
-- * /Capture/ (or /capture group/).  Any substrings of the subject matched
-- by the pattern, meaning the whole pattern and any parenthesized groupings.
-- The PCRE2 docs do not refer to the former as a \"capture\"; however it is
-- accessed the same way in the C API, just with index 0, so we will consider it
-- the 0th capture for consistency.  Parenthesized captures are numbered from 1.
--
-- * /Named capture/.  A parenthesized capture can be named @foo@ like this:
-- @(?\<foo\>...)@.  Whether they have names or not, captures are always
-- implicitly numbered as described above.
--
-- __Performance__
--
-- Each API function is designed such that, when a regex is obtained, the
-- underlying C data generated from the pattern and any options is reused for
-- that regex\'s lifetime.  Care should be taken that the same regex is not
-- recreated /ex nihilo/ and discarded for each new subject:
--
-- > isEmptyOrHas2Digits :: Text -> Bool
-- > isEmptyOrHas2Digits s = Text.null s || matches "\\d{2}" s -- bad, fully applied
--
-- Instead, store it in a partially applied state:
--
-- > isEmptyOrHas2Digits :: Text -> Bool
-- > isEmptyOrHas2Digits = (||) <$> Text.null <*> matches "\\d{2}"
--
-- When in doubt, always create regexes as top-level values:
--
-- > rdbmsFlavor :: Text -> Maybe Text
-- > rdbmsFlavor = matchOpt Caseless "(my|ms)?sql|postgres(ql)?"
--
-- Note: Template Haskell regexes are immune from this problem and may be freely
-- inlined; see below.
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
