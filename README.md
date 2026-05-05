# pcre2

![CI](https://github.com/sjshuck/hs-pcre2/workflows/CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/pcre2)](https://hackage.haskell.org/package/pcre2)

Regular expressions for Haskell.

## Teasers
```haskell
licensePlateRegex :: (Alternative f) => Text -> f Text
licensePlateRegex = match "[A-Z]{3}[0-9]{3,4}"

firstLicensePlate :: Text -> Maybe Text
firstLicensePlate = licensePlateRegex

allLicensePlates :: Text -> [Text]
allLicensePlates = licensePlateRegex
```
```haskell
case "The quick brown fox" of
    [regex|\bbrown\s+(?<animal>[A-z]+)\b|] -> Text.putStrLn animal
    _                                      -> die "nothing brown"
```
```haskell

let kv'd :: Traversal' String (Captures _)
    kv'd = lined . packed . [_regex|(?x)  # Extended PCRE2 syntax
        ^\s*          # Ignore leading whitespace
        ([^=:\s].*?)  # Capture the non-empty key
        \s*           # Ignore trailing whitespace
        [=:]          # Separator
        \s*           # Ignore leading whitespace
        (.*?)         # Capture the possibly-empty value
        \s*$          # Ignore trailing whitespace
    |]

forOf kv'd file $ \kv -> do
    let k = capture @1 kv
        v = capture @2 kv

    Text.putStrLn $ "found " <> k <> " set to " <> v

    kv' <- case Map.lookup k replacements of
        Just v' | v /= v' -> do
            Text.putStrLn $ "setting " <> k <> " to " <> v'
            return $ set (_capture @2) v' kv
        _ -> do
            Text.putStrLn $ "no change for " <> k
            return kv

    ...
```

## Features
* No opaque "`Regex`" object.  Instead, quiet functions with simple
  types&mdash;for the most part it's `Text` _(pattern)_ `-> Text` _(subject)_
  `-> result`.
* No [custom typeclasses](https://hackage.haskell.org/package/regex-base/docs/Text-Regex-Base-RegexLike.html#t:RegexContext).
* A single datatype for both compile and match options, the `Option` monoid.
* Match success expressed via `Alternative`.
* Opt-in Template Haskell facilities for compile-time verification of patterns,
  indexing captures, and memoizing inline regexes.
* Opt-in `lens` support.
* No failure monads to express compile errors, preferring pure functions and
  throwing imprecise exceptions with pretty `Show` instances.  Write simple code
  and debug it.  Or, don't, and use the Template Haskell features instead.  Both
  are first-class.
* Vast presentation of PCRE2 functionality.  We can even register Haskell
  callbacks to run during matching!
* Zero-copying of substrings where beneficial.
* Few dependencies.
* Bundled, statically-linked build of up-to-date PCRE2 (version 10.47), with a
  complete, exposed Haskell binding.

## Performance
Currently (2026-05-02) we are much slower than other libraries.

![Benchmarks graph](https://raw.githubusercontent.com/sjshuck/hs-pcre2/master/bench/bench.svg)

If it's really regex processing that's causing a bottleneck,
[pcre-light](https://hackage.haskell.org/package/pcre-light)/[-heavy](https://hackage.haskell.org/package/pcre-heavy)/[lens-regex-pcre](https://hackage.haskell.org/package/lens-regex-pcre)
are recommended instead of this library for the very best performance.

## Wishlist
* Many performance optimizations.
* Make use of DFA matching for lazy (infinite) inputs.  This likely requires
  some upstream changes as well but in theory it's possible.
* Improve compile time.
  * Support external `libpcre2` maybe.
* Support more types than just `Text`.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html).
PCRE2 is distributed under the [3-clause BSD](https://www.pcre.org/licence.txt) license.

## Main Author
&copy;2020&ndash;2026 Steven Shuck
