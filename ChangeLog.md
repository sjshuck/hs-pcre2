# Changelog and Acknowledgements

## 2.1.1
* Added pattern serialization API, which fixes
  [#23](https://github.com/sjshuck/hs-pcre2/issues/23).
* Updated PCRE2 to 10.40 (no API changes).

## 2.1.0.1
* Explicitly required `text` < 2.
* Minor docs adjustments.

## 2.1.0
* Replaced `Proxy :: Proxy info` with type applications in splices from
  `regex`/`_regex`.  This significantly shortens the splices, producing nicer
  error messages.  As a very minor consequence, we now require the user to turn
  on `{-# LANGUAGE TypeApplications #-}` when using `regex`/`_regex` with
  patterns with parenthesized captures, even when not using
  `capture`/`_capture`.

## 2.0.5
* Enabled PCRE2's built-in Unicode support, which fixes
  [#21](https://github.com/sjshuck/hs-pcre2/issues/21).

## 2.0.4
* Added `Show` instance for `Captures` to ease debugging user code.

## 2.0.3
* Updated PCRE2 to 10.39 (no API changes).  The C sources are now drawn from
  https://github.com/PhilipHazel/pcre2, which fixes
  [#10](https://github.com/sjshuck/hs-pcre2/issues/10).

## 2.0.2
* Fixed a minor issue where the caret indicating pattern location of a
  `Pcre2CompileException` was misplaced if the pattern contained a newline.

## 2.0.1
* Added `microlens` as a dependency to improve Haddock docs (`Traversal'` _et
  al._ are clickable) and relieve maintenance burden somewhat.
* Moderate refactoring of internals.

## 2.0.0
This release introduces significant breaking changes in order to make the API
smaller, more consistent, and safer.
* Implemented [#18](https://github.com/sjshuck/hs-pcre2/issues/18):
    * Removed `matchAll`, `matchAllOpt`, `capturesAll`, and `capturesAllOpt`.
    * Upgraded `match`, `matchOpt`, `captures`, and `capturesOpt` to offer their
      functionality, respectively.
    * Renamed `capturesA` and `capturesAOpt` to `captures` and `capturesOpt`,
      replacing the latter two functions altogether.  `captures`/-`Opt` were
      intended to be extreme
      [convenience functions](https://hackage.haskell.org/package/pcre2-1.1.5/docs/Text-Regex-Pcre2.html#v:captures)
      that required no special datatypes beyond the `Prelude`.  However, this
      was of doubtful benefit, since that's false anyway&mdash;they required
      `Text`, not to mention `{-# LANGUAGE OverloadedStrings #-}`.  Their names
      are simple and valuable, and no other `Alternative`-producing function has
      the naming convention "-`A`", so repurposing their names was in order.
* Moved the callout interface to a new module, `Text.Regex.Pcre2.Unsafe`.  This
  includes the options `UnsafeCompileRecGuard`, `UnsafeCallout`,
  `UnsafeSubCallout`, and `AutoCallout`, and the types `CalloutInfo`,
  `CalloutIndex`, `CalloutResult`, `SubCalloutInfo`, and `SubCalloutResult`.
* Also moved option `BadEscapeIsLiteral` there.
* Removed the ineffectual options `DupNames` and `Utf`.

Other improvements with no API impact:
* Updated PCRE2 to 10.37.
* Replaced copied C files with symlinks, diminishing codebase by 1.5K lines and
  simplifying future PCRE2 updates.
* Reduced size of Template Haskell splices to make error messages less
  obnoxious.
* Moderate refactoring of internals and documentation.

## 1.1.5
* Fixed [#17](https://github.com/sjshuck/hs-pcre2/issues/17), where functions
  returning `Alternative` containers were not restricted to single results
  despite their documentation.
* Minor improvements to docs and examples.

## 1.1.4
* Fixed some incorrect foreign imports' safety.

## 1.1.3.1
* Fixed a very minor issue where `pcreVersion` still reported "10.35" even
  though it in fact was using 10.36.

## 1.1.3
* Made in-house streaming abstraction based on `streaming` and removed the
  latter as a dependency.
* Updated PCRE2 to 10.36 (no API changes).
* Docs fixes.

## 1.1.2
* Refactored using the `streaming` library.  Fixed
  [#11](https://github.com/sjshuck/hs-pcre2/issues/11), where large global
  matches were very slow.

## 1.1.1
* Fixed [#12](https://github.com/sjshuck/hs-pcre2/issues/12), where some
  functions returned too many match results.

## 1.1.0
* Added global matching.
    * New functions `matchAll`, `matchAllOpt`, `capturesAll`, `capturesAllOpt`.
    * Changed all traversals from affine to non-affine.
* Changed `capturesOptA` to `capturesAOpt` for naming consistency.

## 1.0.2
* Fixed [#4](https://github.com/sjshuck/hs-pcre2/4), where multiple named
  captures were not type-indexed correctly.
* Established automated builds using Github Workflows.  Thanks amesgen!

## 1.0.1.1
* Temporarily eliminate all dependency version bounds to get it building on
  Hackage.

## 1.0.1
* Fixed [#1](https://github.com/sjshuck/hs-pcre2/issues/1), where building on
  Windows would succeed but not run.  Thanks Andrew!
* Try to adjust dependency version bounds to get it building on Hackage.  Thanks
  snoyberg!

## 1.0.0
* Initial release.
