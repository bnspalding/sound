{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Sound.Word
-- Description: syllable groupings
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: Experimental
--
-- Sound.Word provides a type 'Word', which is simply a list of syls. It also
-- provides functions for flattening the sounds and stresses of a word into a
-- list.
module Sound.Word where

import qualified Data.Text as T
import Sound.Phoneme (Phoneme (..))
import Sound.Stress (Stress, secondaryStressSymbolIPA, stressSymbolIPA)
import Sound.Syllable as Syl
import Prelude hiding (Word)

-- | a word is a list of syls. It represents a spoken word, or perhaps the
-- pronunciation that would correspond to a written word.
type Word = [Syllable]

-- | sounds combines the flattened sound sets of its syls into a list.
-- The structure (onset-nucleus-coda) of the syls is lost in this
-- transformation.
phonemes :: Word -> [Phoneme]
phonemes = concatMap Syl.phonemes

-- | stresses provides the list of stress levels corresponding to each of its
-- syls.
stresses :: Word -> [Maybe Stress]
stresses = fmap Syl.stress

-- | symbols returns a textual representation of a syllabized word, as a single
-- text object
--
-- Syllables are separated by the character \'.\' (Unicode Full Stop U+002E),
-- except for syllables that begin with an IPA stress mark, which serves as a
-- syllable separator in place of the dot.
symbols :: Word -> T.Text
symbols syls = T.concat $ head syms : (markBreak <$> tail syms)
  where
    syms = Syl.symbols <$> syls
    -- prepend a syl break symbol in cases where there is not a stress mark
    markBreak syl =
      if T.head syl `elem` [stressSymbolIPA, secondaryStressSymbolIPA]
        then syl
        else "." <> syl
