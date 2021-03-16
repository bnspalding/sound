-- |
-- Module: Rhyme.Approx
-- Description: approximate rhyme
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Rhyme.Approx provides tools for determing (approximately) how close two
-- syllables are by measure of rhyme, assonance, or alliteration. In all cases,
-- similarity between syllables is based on what fraction of the total features
-- of the two syllables' sounds overlap.
--
-- The feature sets for sounds are taken from "Sound.GenAm", which means that the
-- tools are currently constrained to a particular, semi-arbitrary mapping.
--
-- Currently, ordering of sounds is not considered in measuring the similarity.
-- Instead, a \'bag of sounds\' approach is taken, and this should be taken into
-- consideration when evaluating what similarity means.
module Rhyme.Approx
  ( -- * Measures
    rhyme,
    assonance,
    alliteration,

    -- * Similarity
    similarity,
  )
where

import qualified Data.HashSet as HashSet
import Data.Ratio
import Sound.Feature
import Sound.Phoneme
import Sound.Syllable hiding (rhyme)
import qualified Sound.Syllable as Syl (rhyme)

-- | rhyme generates a measure of similarity between two syllables by comparing
-- the rhymes of the syllables (nucleus + coda) together as flat lists.
rhyme :: Syllable -> Syllable -> Rational
rhyme = _similarity Syl.rhyme

-- | assonance generates a measure of similarity between two syllables by
-- comparing only their nuclei. The nucleus is the most sonorous sound
-- (generally a vowel) at the center of a syllable.
assonance :: Syllable -> Syllable -> Rational
assonance = _similarity (\x -> [nucleus x])

-- | alliteration generates a measure of similarity between two syllables by
-- comparing their onsets. The onset is the cluster of sounds that begin a
-- syllable, up until the nucleus.
alliteration :: Syllable -> Syllable -> Rational
alliteration = _similarity onset

-- | similarity compares two sets of sounds and returns a measure of similarity
-- (a number between 0 and 1). The similarity measure is the fraction of
-- features shared by the two sets of sounds over the total number of unique
-- sounds in the two sound sets.
similarity :: [Phoneme] -> [Phoneme] -> Rational
similarity ss1 ss2 =
  fromIntegral (HashSet.size $ HashSet.intersection fs1 fs2)
    % fromIntegral (HashSet.size $ HashSet.union fs1 fs2)
  where
    fs1 = _merge $ features <$> ss1
    fs2 = _merge $ features <$> ss2

_similarity :: (Syllable -> [Phoneme]) -> Syllable -> Syllable -> Rational
_similarity f syl1 syl2 = similarity (f syl1) (f syl2)

_merge :: [FeatureSet] -> FeatureSet
_merge = foldl HashSet.union HashSet.empty
