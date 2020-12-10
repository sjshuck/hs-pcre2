# pcre2

![CI](https://github.com/sjshuck/hs-pcre2/workflows/CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/pcre2)](https://hackage.haskell.org/package/pcre2)

Regular expressions for Haskell.  
https://hackage.haskell.org/package/pcre2

## Teasers
```haskell
embeddedLicensePlate :: Text -> Maybe Text
embeddedLicensePlate = match "[A-Z]{3}[0-9]{3,4}"
```
```haskell
case "The quick brown fox" of
    [regex|brown\s+(?<animal>\S+)|] -> Text.putStrLn animal
    _                               -> error "nothing brown"
```
```haskell
let kv'd = lined . packed . [_regex|^\s*(.*?)\s*[=:]\s*(.*)|]

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
* Low-surface API covering most use cases.
* Quiet functions with simple types&mdash;for the most part it's  
`Text` _(pattern)_ `-> Text` _(subject)_ `-> result`.
* Use partial application to create performant, compile-once-match-many code.
* Low cognitive overhead&mdash;there's just one custom datatype for both compile
  and match options, the `Option` monoid.
* `Text` everywhere for interop with both C and the broader Haskell ecosystem.
* Match failures expressed via `Alternative` or pattern match failures.
* Opt-in Template Haskell facilities for compile-time verification of patterns,
  indexing captures, and memoizing inline regexes.
* Opt-in `lens` support.
* No failure monads to express compile errors, preferring pure functions and
  throwing imprecise exceptions with pretty `Show` instances.  Write simple
  code and debug it.  Or, don't, and use the Template Haskell features instead.
  Both are first-class.
* Vast presentation of PCRE2 functionality.  We can even register Haskell
  callbacks to run during matching!
* No dependencies that aren't distributed with GHC.
* Bundled, statically-linked UTF-16 build of ~~up-to-date~~ PCRE2 (version
  10.35), with a complete, exposed Haskell binding.

## TODO
* Global matching.  (We already have global substitution.)
* Many performance optimizations.  Currently we are 2&ndash;3&times; slower
  than other libraries doing everything (a few &mu;s).
* Make use of DFA and JIT compilation.
* Improve PCRE2 C compile time.
* PCRE2 10.36 is out already.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html).  
PCRE2 is distributed under the [3-clause BSD](https://www.pcre.org/licence.txt) license.

## Author
&copy;2020 Shlomo Shuck