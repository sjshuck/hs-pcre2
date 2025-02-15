module Text.Regex.Pcre2 (
    -- * Matching and substitution
    {-|
    === __Introduction__

    Atop the low-level binding to the C API, we present a high-level interface
    to add regular expressions to Haskell programs.

    All input and output strings are strict `Data.Text.Text`, which maps to how
    PCRE2 operates in UTF-8 mode.

    The C API requires pattern strings to be compiled and the compiled patterns
    to be executed on subject strings in discrete steps.  We simplify this
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
    configurable variants; the latter have "@Opt@" in the name and accept a
    value of this type.

    Similar to how @head :: [a] -> a@ sacrifices totality for type simplicity,
    we represent user errors as imprecise exceptions.  Unlike with @head@, these
    exceptions are typed (as `SomePcre2Exception`s); moreover, we offer Template
    Haskell facilities that can intercept some of these errors before the
    program is run.  (Failure to match is not considered a user error and is
    represented by `Control.Applicative.empty`; see below.)

    [There's more than one way to do it](https://en.wikipedia.org/wiki/There's_more_than_one_way_to_do_it)
    with this library.  The choices between functions and traversals,
    poly-kinded `Captures` and plain lists, string literals and
    quasi-quotations, quasi-quoted expressions and quasi-quoted patterns...these
    are left to the user.  She will observe that advanced features' extra
    safety, power, and convenience entail additional language extensions,
    cognitive overhead, and (for lenses) library dependencies, so it's really a
    matter of finding the best trade-offs for her case.

    === __Definitions__

    [Pattern]:  The string defining a regular expression.  Refer to
    syntax [here](https://pcre.org/current/doc/html/pcre2pattern.html).

    [Subject]:  The string the compiled regular expression is executed on.

    [Regex]:  A function of the form @`Data.Text.Text` -> result@, where the
    argument is the subject.  It is \"compiled\" in the course of producing
    results and may be reused via partial application as discussed above.  (Lens
    users:  A regex has the more abstract form @`Lens.Micro.Traversal'`
    `Data.Text.Text` result@, but the concept is the same.)

    [Capture (or capture group)]:  Any substrings of the subject matched by the
    pattern, meaning the whole pattern and any parenthesized groupings.  The
    PCRE2 docs do not refer to the former as a \"capture\"; however it is
    accessed the same way in the C API, just with index 0, so we will consider
    it the 0th capture for consistency.  Parenthesized captures are implicitly
    numbered from 1.

    [Unset capture]:  A capture considered unset as distinct from empty.  This
    can arise from matching the pattern @(a)?@ to an empty subject&#x2014;the
    0th capture will be set as empty, but the 1st will be unset altogether.  We
    represent both as empty `Data.Text.Text` for simplicity.  See below for
    discussions about how unset captures may be detected or substituted using
    this library.

    [Named capture]:  A parenthesized capture can be named like this:
    @(?\<foo\>...)@.  Whether they have names or not, captures are always
    numbered as described above.

    === __Handling results and errors__

    In contrast to [other](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)
    [APIs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/matchAll)
    where there are separate functions to request single versus global matching,
    we accomplish this in a unified fashion using the
    `Control.Applicative.Alternative` typeclass /(since 2.0.0)/.  Typically the
    user will choose from two instances, `Maybe` and @[]@:

    > b2 :: (Alternative f) => Text -> f Text
    > b2 = match "b.."
    >
    > -- Zero or one match
    > findB2 :: Text -> Maybe Text
    > findB2 = b2
    >
    > -- Zero or more matches
    > findAllB2s :: Text -> [Text]
    > findAllB2s = b2

    Other instances exist for niche uses, such as @[STM](https://hackage.haskell.org/package/stm/docs/Control-Monad-STM.html#t:STM)@.

    By contrast, user errors are thrown purely.  If a user error is to be
    caught, it must be at the site where the match or substitution results are
    evaluated.  As a particular consequence, pattern compile errors are deferred
    to match sites.

    >>> broken = match "*"
    >>> :t broken
    broken :: Alternative f => Text -> f Text
    >>> broken "foo"
    *** Exception: pcre2_compile: quantifier does not follow a repeatable item
                        *
                        ^

    `Control.Exception.evaluate` comes in handy to force results into the `IO`
    monad in order to catch errors reliably:

    >>> :set -XTypeApplications
    >>> handle @SomePcre2Exception (\_ -> return Nothing) $ evaluate $ broken "foo"
    Nothing
    -}

    -- ** Basic matching functions
    match,
    matchOpt,
    matches,
    matchesOpt,
    captures,
    capturesOpt,

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
    -- [this](https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html),
    -- and videos such as [this](https://www.youtube.com/watch?v=geV8F59q48E).
    --
    -- 2.  A library providing combinators.  For lens newcomers, it is
    -- recommended to grab
    -- [microlens-platform](https://hackage.haskell.org/package/microlens-platform)&#x2014;all
    -- of the examples in this library work with it,
    -- @[packed](https://hackage.haskell.org/package/microlens-platform/docs/Lens-Micro-Platform.html#v:packed)@
    -- and
    -- @[unpacked](https://hackage.haskell.org/package/microlens-platform/docs/Lens-Micro-Platform.html#v:packed)@
    -- are included for working with `Data.Text.Text`, and it is
    -- upwards-compatible with the
    -- full [lens](https://hackage.haskell.org/package/lens) library.
    --
    -- We expose a set of traversals that focus on matched substrings within a
    -- subject.
    --
    -- > _nee :: Traversal' Text Text
    -- > _nee = _matchOpt (Caseless <> MatchWord) "nee"
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
    -- | The API thus far has some hazards:
    --
    -- * pattern malformation such as mismatched parentheses /(runtime error)/
    --
    -- * out-of-bounds indexing of a capture group list /(runtime error)/
    --
    -- * out-of-bounds `Lens.Micro.ix`ing of a `Lens.Micro.Traversal'` target
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
    -- features, we provide a Template Haskell API to mitigate these.  To make
    -- use of it these must be enabled:
    --
    -- +--------------------+---------------------------------------+------------------------------+
    -- | Extension          | Required for                          | When                         |
    -- +====================+=======================================+==============================+
    -- | @DataKinds@        | `GHC.TypeLits.Nat`s (numbers),        | Using `regex`\/`_regex` with |
    -- |                    | `GHC.TypeLits.Symbol`s (strings), and | a pattern containing         |
    -- |                    | other type-level data powering        | parenthesized captures       |
    -- |                    | compile-time capture lookups          |                              |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @QuasiQuotes@      | @[@/f/@|@...@|]@ syntax               | Using `regex`\/`_regex`      |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @TypeApplications@ | @\@i@ syntax for supplying type index | Using `regex`\/`_regex` with |
    -- |                    | arguments to applicable functions     | a pattern containing         |
    -- |                    |                                       | parenthesized captures;      |
    -- |                    |                                       | using `capture`\/`_capture`  |
    -- +--------------------+---------------------------------------+------------------------------+
    -- | @ViewPatterns@     | Running code and binding variables in | Using `regex` as a Haskell   |
    -- |                    | pattern context proper (pattern       | pattern                      |
    -- |                    | guards are off-limits for this)       |                              |
    -- +--------------------+---------------------------------------+------------------------------+
    --
    -- The inspiration for this portion of the library is Ruby, which supports
    -- regular expressions with [superior ergonomics](https://docs.ruby-lang.org/en/master/Regexp.html#class-Regexp-label-Named+Captures).

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
        Bsr,
        Caseless,
        DepthLimit,
        DollarEndOnly,
        DotAll,
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
        Ungreedy),

    Bsr(..),
    Newline(..),

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

import Text.Regex.Pcre2.Internal
import Text.Regex.Pcre2.TH
