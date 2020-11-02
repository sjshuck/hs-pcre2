{-# LANGUAGE RankNTypes #-}

module Text.Regex.Pcre2 where

import Control.Applicative       (Alternative(..))
import Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.List.NonEmpty        as NE
import Data.Text                 (Text)
import Text.Regex.Pcre2.Internal

-- | Match a pattern to a subject and return a list of captures, or @[]@ if no
-- match.
captures :: Text -> Text -> [Text]
captures = capturesOpt mempty

-- | @captures = capturesOpt mempty@
capturesOpt :: Option -> Text -> Text -> [Text]
capturesOpt option patt = view $ _capturesOpt option patt . to NE.toList

-- | Match a pattern to a subject and return a non-empty list of captures in an
-- `Alternative`, or `empty` if no match.  Typically the @Alternative@ instance
-- will be `Maybe`, but other useful ones exist, notably those of `STM` and of
-- the various parser combinator libraries.
--
-- Note that PCRE2 errors are distinct from match failures and are not
-- represented as `empty`; they are thrown purely as `Pcre2Exception`s.
-- Returning @IO (NonEmpty Text)@ from this function will not enable you to
-- catch them; you must
--
-- > let parseDate = capturesA "(\\d{4})-(\\d{2})-(\\d{2})"
-- > in case parseDate "submitted 2020-10-20" of
-- >     Just (date :| [y, m, d]) -> ...
-- >     Nothing                  -> putStrLn "didn't match"
capturesA :: (Alternative f) => Text -> Text -> f (NonEmpty Text)
capturesA = capturesOptA mempty

-- | @capturesA = capturesOptA mempty@
capturesOptA :: (Alternative f) => Option -> Text -> Text -> f (NonEmpty Text)
capturesOptA option patt = maybe empty pure . preview (_capturesOpt option patt)

-- | Does the pattern match the subject?
matches :: Text -> Text -> Bool
matches = matchesOpt mempty

-- | @matches = matchesOpt mempty@
matchesOpt :: Option -> Text -> Text -> Bool
matchesOpt option patt = has $ _capturesOpt option patt

-- | Match a pattern to a subject and return only the portion that matched, i.e.
-- the zeroth capture.
match :: (Alternative f) => Text -> Text -> f Text
match = matchOpt mempty

matchOpt :: (Alternative f) => Option -> Text -> Text -> f Text
matchOpt = withMatcher $ \matcher ->
    let _match = _capturesInternal matcher (Just $ 0 :| []) . _headNE
    in maybe empty pure . preview _match

-- | Given a pattern, produce an affine traversal (0 or 1 targets) that focuses
-- from a subject to a potential non-empty list of captures.
--
-- Setting works in the following way:  If a capture is changed such that the
-- new `Text` is not equal to the old one, a substitution occurs, otherwise it
-- doesn\'t.  This matters in cases where a capture encloses another
-- capture&mdash;notably, _all_ parenthesized captures are enclosed by the 0th
-- capture, the region of the subject matched by the whole pattern.
--
-- > let threeAndMiddle = _captures ". (.) ."
-- > print $ set threeAndMiddle ("A A A" :| ["B"]) "A A A" -- "A B A"
-- > print $ set threeAndMiddle ("A B A" :| ["A"]) "A A A" -- "A B A"
--
-- Changing multiple overlapping captures is unsupported and won\'t do what you
-- want.
--
-- If the list becomes longer for some reason, the extra elements are ignored;
-- if it\'s shortened, the absent elements are considered to be unchanged.
--
-- It's recommend that the list be modified capture-wise, using @ix@.
--
-- > let madlibs = _captures "(\\w+) my (\\w+)"
-- >
-- > print $ "Well bust my buttons!" &~ do
-- >     zoom madlibs $ do
-- >         ix 1 . _head .= 'd'
-- >         ix 2 %= Text.reverse
-- >     _last .= '?'
-- >
-- > -- "Well dust my snottub?"
_captures :: Text -> Traversal' Text (NonEmpty Text)
_captures = _capturesOpt mempty

_capturesOpt :: Option -> Text -> Traversal' Text (NonEmpty Text)
_capturesOpt = withMatcher $ \matcher -> _capturesInternal matcher Nothing