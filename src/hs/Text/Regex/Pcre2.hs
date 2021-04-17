module Text.Regex.Pcre2 (
    -- * Matching and substitution
    {-|
    /__Introduction__/

    Atop the low-level binding to the C API, we present a high-level interface
    to add regular expressions to Haskell programs.

    All input and output strings are strict `Text`, which maps directly to how
    PCRE2 operates on strings of 16-bit-wide code units.

    The C API requires pattern strings to be compiled and the compiled patterns
    to be executed on subject strings in discrete steps.  We hide this
    procedure, accepting pattern and subject as arguments in a single function,
    essentially:

    > pattern -> subject -> result

    The implementation guarantees that,
    when [partially applied](https://wiki.haskell.org/Partial_application) to
    pattern but not subject, the resulting function
    will [close](https://en.wikipedia.org/wiki/Closure_(computer_programming\))
    on the underlying compiled object and reuse it for every subject it is
    subsequently applied to.

    Likewise, we do not require the user to know whether a PCRE2 option is to be
    applied at pattern compile time or match time.  Instead we fold all possible
    options into a single datatype, `Option`.  Most functions have vanilla and
    configurable variants; the latter have \"@Opt@\" in the name and accept a
    value of this type.

    Similar to how @head :: [a] -> a@ sacrifices totality for type simplicity,
    we represent user errors as imprecise exceptions.  Unlike with @head@, these
    exceptions are typed (as `SomePcre2Exception`s); moreover, we offer Template
    Haskell facilities that can intercept some of these errors before the
    program is run.  (Failure to match is not considered a user error and is
    represented in the types.)

    [There's more than one way to do it](https://en.wikipedia.org/wiki/There's_more_than_one_way_to_do_it)
    with this library.  The choices between functions and traversals,
    poly-kinded `Captures` and plain lists, string literals and
    quasi-quotations, quasi-quoted expressions and quasi-quoted patterns...these
    are left to the user.  She will observe that advanced features\' extra
    safety, power, and convenience entail additional language extensions,
    cognitive overhead, and (for lenses) library dependencies, so it\'s really a
    matter of finding the best trade-offs for her case.

    /__Definitions__/

    [Pattern]:  The string defining a regular expression.  Refer to
    syntax [here](https://pcre.org/current/doc/html/pcre2pattern.html).

    [Subject]:  The string the compiled regular expression is executed on.

    [Regex]:  A function of the form @`Text` -> result@, where the argument is
    the subject.  It is \"compiled\" via partial application as discussed above.
    (Lens users:  A regex has the more abstract form
    @[Traversal\'](https://hackage.haskell.org/package/microlens/docs/Lens-Micro.html#t:Traversal-39-)
    `Text` result@, but the concept is the same.)

    [Capture (or capture group)]:  Any substrings of the subject matched by the
    pattern, meaning the whole pattern and any parenthesized groupings.  The
    PCRE2 docs do not refer to the former as a \"capture\"; however it is
    accessed the same way in the C API, just with index 0, so we will consider
    it the 0th capture for consistency.  Parenthesized captures are implicitly
    numbered from 1.

    [Unset capture]:  A capture considered unset as distinct from empty.  This
    can arise from matching the pattern @(a)?@ to an empty subject&#x2014;the
    0th capture will be set as empty, but the 1st will be unset altogether.  We
    represent both as empty `Text` for simplicity.  See below for discussions
    about how unset captures may be detected or substituted using this library.

    [Named capture]:  A parenthesized capture can be named like this:
    @(?\<foo\>...)@.  Whether they have names or not, captures are always
    numbered as described above.

    /__Performance__/

    Each API function is designed such that, when a regex is obtained, the
    underlying C data generated from the pattern and any options is reused for
    that regex\'s lifetime.  Care should be taken that the same regex is not
    recreated /ex nihilo/ and discarded for each new subject:

    > isEmptyOrHas2Digits :: Text -> Bool
    > isEmptyOrHas2Digits s = Text.null s || matches "\\d{2}" s -- bad, fully applied

    Instead, store it in a partially applied state:

    > isEmptyOrHas2Digits = (||) <$> Text.null <*> matches "\\d{2}" -- OK but abstruse

    When in doubt, always create regexes as top-level values:

    > has2Digits :: Text -> Bool
    > has2Digits = matches "\\d{2}"
    >
    > isEmptyOrHas2Digits s = Text.null s || has2Digits s -- good

    Note: Template Haskell regexes are immune from this problem and may be
    freely inlined; see below.

    /__Handling errors__/

    In a few places we use the `Alternative` typeclass to optionally return
    match results, expressing success via `pure` and failure via `empty`.
    Typically the user will choose the instance `Maybe`, but other useful ones
    exist, notably @[STM](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#t:STM)@,
    that of [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative.html#t:Parser),
    and those of parser combinator libraries such
    as [megaparsec](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#t:ParsecT).

    By contrast, user errors are thrown purely.  If a user error is to be
    caught, it must be at the site where the match or substitution results are
    evaluated&#x2014;in other words, wherever the regex is applied to a subject.
    Even pattern compile errors are deferred to match sites, due to the way this
    library employs `System.IO.Unsafe.unsafePerformIO` to implement laziness.

    >>> broken = match "*"
    >>> broken "foo"
    *** Exception: pcre2_compile: quantifier does not follow a repeatable item
                        *
                        ^

    `Control.Exception.evaluate` comes in handy to force results into the `IO`
    monad in order to catch errors reliably:

    >>> evaluate (broken "foo") `catch` \(_ :: SomePcre2Exception) -> return Nothing
    Nothing

    Or simply select `IO` as the `Alternative` instance:

    >>> :t broken
    broken :: Alternative f => Text -> f Text
    >>> broken "foo" `catch` \(_ :: SomePcre2Exception) -> return "broken"
    "broken"
    -}

    -- ** Basic matching functions
    match,
    matchOpt,
    matchAll,
    matchAllOpt,
    matches,
    matchesOpt,
    captures,
    capturesOpt,
    capturesA,
    capturesAOpt,
    capturesAll,
    capturesAllOpt,

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
    -- [microlens-platform](https://hackage.haskell.org/package/microlens-platform)&#x2014;most
    -- of the examples in this library work with it,
    -- @[packed](https://hackage.haskell.org/package/microlens-platform/docs/Lens-Micro-Platform.html#v:packed)@
    -- and
    -- @[unpacked](https://hackage.haskell.org/package/microlens-platform/docs/Lens-Micro-Platform.html#v:packed)@
    -- are included for working with `Text`, and it is upwards-compatible with
    -- the full [lens](https://hackage.haskell.org/package/lens) library.
    --
    -- We expose a set of traversals that focus on matched substrings within a
    -- subject.  Like the basic functional regexes, they should be \"compiled\"
    -- and memoized, rather than created inline.
    --
    -- > _nee :: Traversal' Text Text
    -- > _nee = _match "(?i)\\bnee\\b"
    --
    -- In addition to getting results, they support global substitution through
    -- setting; more generally, they can accrete effects while performing
    -- replacements.
    --
    -- >>> promptNee = traverseOf (_nee . unpacked) $ \s -> print s >> getLine
    -- >>> promptNee "We are the knights who say...NEE!"
    -- "NEE"
    -- NOO
    -- "We are the knights who say...NOO!"
    -- >>>
    --
    -- In general these traversals are not law-abiding.
    _match,
    _matchOpt,
    _captures,
    _capturesOpt,

    -- * Compile-time validation
    --
    -- | Despite whatever virtues, the API thus far has some fragility arising
    -- from various scenarios:
    --
    -- * pattern malformation such as mismatched parentheses /(runtime error)/
    --
    -- * out-of-bounds indexing of a capture group list /(runtime error)/
    --
    -- * out-of-bounds @ix@ing of a @Traversal\'@ target
    -- /(spurious failure to match)/
    --
    -- * case expression containing a Haskell list pattern of the wrong length
    -- /(spurious failure to match)/
    --
    -- * regex created and discarded inline /(suboptimal performance)/
    --
    -- * precariously many backslashes in a pattern.  Matching a literal
    -- backslash requires the sequence @\"\\\\\\\\\"@!
    --
    -- Using a combination of language extensions and pattern introspection
    -- features, we provide a Template Haskell API to mitigate these scenarios.
    -- To make use of it these must be enabled:
    --
    -- +--------------------+---------------------------------------+------------------------------+
    -- | Extension          | Required for                          | When                         |
    -- +====================+=======================================+==============================+
    -- | @DataKinds@        | `GHC.TypeLits.Nat`s (numbers),        | Using `regex`\/`_regex` with |
    -- |                    | `GHC.TypeLits.Symbol`s (strings), and | a pattern containing         |
    -- |                    | other type-level data powering        | parenthesized captures       |
    -- |                    | compile-time capture lookups          |                              |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @QuasiQuotes@      | @[@/f/@|@...@|]@ syntax               | Always                       |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @TypeApplications@ | @\@i@ syntax for supplying type index | Using `capture`\/`_capture`  |
    -- |                    | arguments to applicable functions     |                              |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @ViewPatterns@     | Running code and binding variables in | Using `regex` as a Haskell   |
    -- |                    | pattern context proper (pattern       | pattern                      |
    -- |                    | guards are off-limits for this)       |                              |
    -- +--------------------+---------------------------------------+------------------------------+
    --
    -- The inspiration for this portion of the library is Ruby, which supports
    -- regular expressions with [superior ergonomics](https://ruby-doc.org/core-2.7.2/Regexp.html#class-Regexp-label-Capturing).

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
        SubUnknownUnset,
        SubUnsetEmpty,
        Ucp,
        Ungreedy,
        UnsafeCallout,
        UnsafeCompileRecGuard,
        UnsafeSubCallout,
        Utf),

    Bsr(..),
    Newline(..),

    -- ** Callout interface

    CalloutInfo(..),
    CalloutIndex(..),
    CalloutResult(..),
    -- *** Substitution callouts
    SubCalloutInfo(..),
    SubCalloutResult(..),

    -- * User errors
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

import Control.Applicative       (Alternative(..))
import Data.Text                 (Text)
import Text.Regex.Pcre2.Internal
import Text.Regex.Pcre2.TH
