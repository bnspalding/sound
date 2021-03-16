-- |
-- Module: Rhyme.Strict
-- Description: strict rhyme
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Rhyme.Strict provides tools for determing if two syllables match by measure
-- of rhyme, assonance, or alliteration. In all cases, a subset of sounds from
-- the syllables, selected by the measure used, are compared for equality. Two
-- syllables either match by some measure, or they do not.
module Rhyme.Strict (rhyme, assonance, alliteration) where

import Sound.Syllable hiding (rhyme)
import qualified Sound.Syllable as Syl (rhyme)

-- | rhyme compares the rhymes (nucleus + coda) of two syllables to each other.
rhyme :: Syllable -> Syllable -> Bool
rhyme syl1 syl2 = Syl.rhyme syl1 == Syl.rhyme syl2

-- | assonance compares the nuclei of two syllables to each other. The nucelus
-- is the most sonorous sound (generally a vowel) at the center of a syllable.
assonance :: Syllable -> Syllable -> Bool
assonance syl1 syl2 = nucleus syl1 == nucleus syl2

-- | alliteration compares the onsets of two syllables to each other. The onset
-- is the cluster of sounds that begin a syllable, up until the nucleus.
alliteration :: Syllable -> Syllable -> Bool
alliteration syl1 syl2 = onset syl1 == onset syl2
