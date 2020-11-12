-- | High-level Haskell facilities for PCRE2 regular expressions.
module Text.Regex.Pcre2 (
    -- * Matching and substitution

    -- ** Basic matching functions
    captures,
    capturesOpt,
    capturesA,
    capturesOptA,
    match,
    matchOpt,
    matches,
    matchesOpt,
    -- ** PCRE2-native substitution
    sub,
    gsub,
    subOpt,
    -- ** Lens-powered matching and substitution
    _captures,
    _capturesOpt,
    _match,
    _matchOpt,

    -- * Compile-time validation

    -- ** Template Haskell facilities
    re,
    _re,
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

import Text.Regex.Pcre2.Internal
import Text.Regex.Pcre2.TH
