# pcre2

## Teaser
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text.IO        as Text
import           Text.Regex.Pcre2.TH (re)

main = case "The quick brown fox" of
    [re|brown\s+(?<animal>\S+)|] -> Text.putStrLn animal
    _                            -> error "nothing brown"
```

## Features
* Fast.
* Low-surface API covering most use cases.
* Quiet, simple functions&mdash;for the most part it's just
  ```
  pattern -> subject -> results
  ```
* Easy and predictable way to leverage partial application to create performant,
  compile-once-match-many code.
* Low cognitive overhead&mdash;there's just one custom datatype for both compile
  and match options, the `Option` monoid.
* Powerful Template Haskell facilities for compile-time verification of
  patterns, consuming named captures, and memoizing inline regexes.
* No failure monads to express compile errors, preferring pure functions and
  throwing unsafe, pure exceptions with pretty `Show` instances.  Write simple
  code and debug it.  Or, don't, and use the Template Haskell features instead.
  Both are first-class.
* Match failures expressed via `Alternative` or pattern match failures.
* Vast presentation of PCRE2 functionality.  We can even register Haskell
  callbacks to run during matching!
* Limited dependencies.  No `regex-base`.
* Bundled, statically-linked UTF-16 build of up-to-date `libpcre2`.
* `Text` everywhere, the sweet spot for interop with the underlying C library
  and with the broader Haskell ecosystem.
* Opt-in `lens`/`microlens` support.
  ```haskell
  let kv'd = lined . packed . [_re|^\s*(.*?)\s*[=:]\s*(.*)|] . substrings
  forMOf kv'd file $ \(k, v) -> Text.putStrLn $ k <> " set to " <> v
  ```