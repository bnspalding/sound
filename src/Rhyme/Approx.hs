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

import qualified Data.Set as Set
import Sound
import Sound.Feature
import qualified Sound.GenAm as GenAm
import qualified Sound.Syl as Syl

-- | rhyme generates a measure of similarity between two syllables by comparing
-- the rhymes of the syllables (nucleus + coda) together as flat lists.
rhyme :: Syl -> Syl -> Float
rhyme = _similarity Syl.rhyme

-- | assonance generates a measure of similarity between two syllables by
-- comparing only their nuclei. The nucleus is the most sonorous sound
-- (generally a vowel) at the center of a syllable.
assonance :: Syl -> Syl -> Float
assonance = _similarity Syl.nucleus

-- | alliteration generates a measure of similarity between two syllables by
-- comparing their onsets. The onset is the cluster of sounds that begin a
-- syllable, up until the nucleus.
alliteration :: Syl -> Syl -> Float
alliteration = _similarity Syl.onset

-- | similarity compares two sets of sounds and returns a measure of similarity
-- (a number between 0 and 1). The similarity measure is the fraction of
-- features shared by the two sets of sounds over the total number of unique
-- sounds in the two sound sets.
similarity :: [Sound] -> [Sound] -> Float
similarity ss1 ss2 =
  fromIntegral (Set.size $ Set.intersection fs1 fs2)
    / fromIntegral (Set.size $ Set.union fs1 fs2)
  where
    fs1 = _merge $ _featuresOf ss1
    fs2 = _merge $ _featuresOf ss2

_similarity :: (Syl -> [Sound]) -> Syl -> Syl -> Float
_similarity f syl1 syl2 = similarity (f syl1) (f syl2)

_featuresOf :: [Sound] -> [FeatureSet]
_featuresOf ss = featuresOrEmpty . GenAm.features <$> ss

_merge :: [FeatureSet] -> FeatureSet
_merge = foldl Set.union Set.empty
-- TODO: revisit the type of number that should be returned by similarity. is it
-- a ratio? a broader typeclass? Look at what is both most conventional and what
-- is most correct.
