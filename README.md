# pcre2

![CI](https://github.com/sjshuck/hs-pcre2/workflows/CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/pcre2)](https://hackage.haskell.org/package/pcre2)

Regular expressions for Haskell.  

## Teasers
```haskell
licensePlate :: Text -> Maybe Text
licensePlate = match "[A-Z]{3}[0-9]{3,4}"

licensePlates :: Text -> [Text]
licensePlates = match "[A-Z]{3}[0-9]{3,4}"
```
```haskell
case "The quick brown fox" of
    [regex|\bbrown\s+(?<animal>[A-z]+)\b|] -> Text.putStrLn animal
    _                                      -> error "nothing brown"
```
```haskell
let kv'd = lined . packed . [_regex|(?x)  # Extended PCRE2 syntax
        ^\s*          # Ignore leading whitespace
        ([^=:\s].*?)  # Capture the non-empty key
        \s*           # Ignore trailing whitespace
        [=:]          # Separator
        \s*           # Ignore leading whitespace
        (.*?)         # Capture the possibly-empty value
        \s*$          # Ignore trailing whitespace
    |]

forMOf kv'd file $ execStateT $ do
    k <- gets $ capture @1
    v <- gets $ capture @2
    liftIO $ Text.putStrLn $ "found " <> k <> " set to " <> v

    case myMap ^. at k of
        Just v' | v /= v' -> do
            liftIO $ Text.putStrLn $ "setting " <> k <> " to " <> v'
            _capture @2 .= v'
        _ -> liftIO $ Text.putStrLn "no change"
```

## Features
* No opaque "`Regex`" object.  Instead, quiet functions with simple
  types&mdash;for the most part it's `Text` _(pattern)_ `-> Text` _(subject)_
  `-> result`.  Use partial application to create performant,
  compile-once-match-many code.
* No [custom typeclasses](https://hackage.haskell.org/package/regex-base/docs/Text-Regex-Base-RegexLike.html#t:RegexContext).
* A single datatype for both compile and match options, the `Option` monoid.
* `Text` everywhere.
* Match success expressed via `Alternative`.
* Opt-in Template Haskell facilities for compile-time verification of patterns,
  indexing captures, and memoizing inline regexes.
* Opt-in `lens` support.
* No failure monads to express compile errors, preferring pure functions and
  throwing imprecise exceptions with pretty `Show` instances.  Write simple
  code and debug it.  Or, don't, and use the Template Haskell features instead.
  Both are first-class.
* Vast presentation of PCRE2 functionality.  We can even register Haskell
  callbacks to run during matching!
* Zero-copying of substrings where beneficial.  Benchmarks show a 10&times;
  speedup over `pcre-light`, and 20&times; over `regex-pcre`, for longer
  captures.
* Few dependencies.
* Bundled, statically-linked UTF-8 build of up-to-date PCRE2 (version 10.40),
  with a complete, exposed Haskell binding.

## Wishlist
* Many performance optimizations.  Currently we are as much as 2&ndash;3&times;
  slower than other libraries for some operations, although things are
  improving.  If it's really regex processing that's causing a bottleneck,
  [pcre-light](https://hackage.haskell.org/package/pcre-light)/[-heavy](https://hackage.haskell.org/package/pcre-heavy)/[lens-regex-pcre](https://hackage.haskell.org/package/lens-regex-pcre)
  are recommended instead of this library for the very best performance.
* Make use of DFA matching and JIT compilation.
* Improve PCRE2 C compile time.
* Add splitting support.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html).  
PCRE2 is distributed under the [3-clause BSD](https://www.pcre.org/licence.txt) license.

## Main Author
&copy;2020&ndash;2022 Shlomo Shuck
