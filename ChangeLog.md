# Changelog and Acknowledgements

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
  [#11](https://github.com/sjshuck/pcre2/11), where large global matches were
  very slow.

## 1.1.1
* Fixed [#12](https://github.com/sjshuck/pcre2/4), where some functions returned
  too many match results.

## 1.1.0
* Added global matching.
    * New functions `matchAll`, `matchAllOpt`, `capturesAll`, `capturesAllOpt`.
    * Changed all traversals from affine to non-affine.
* Changed `capturesOptA` to `capturesAOpt` for naming consistency.

## 1.0.2
* Fixed [#4](https://github.com/sjshuck/pcre2/4), where multiple named captures
  were not type-indexed correctly.
* Established automated builds using Github Workflows.  Thanks amesgen!

## 1.0.1.1
* Temporarily eliminate all dependency version bounds to get it building on
  Hackage.

## 1.0.1
* Fixed [#1](https://github.com/sjshuck/pcre2/1), where building on Windows
  would succeed but not run.  Thanks Andrew!
* Try to adjust dependency version bounds to get it building on Hackage.  Thanks
  snoyberg!

## 1.0.0
* Initial release.
