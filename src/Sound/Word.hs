-- |
-- Module: Sound.Word
-- Description: syllable groupings
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: Experimental
--
-- Sound.Word provides a type Word, which is simply a list of syls. It also
-- provides functions for flattening the sounds and stresses of a word into a
-- simple list.
module Sound.Word where

import Sound.Sound
import Sound.Stress
import Sound.Syl as Syl

-- | a word is a list of syls. It represents a spoken word, or perhaps the
-- pronunciation that would correspond to a written word.
type Word = [Syl]

-- | sounds combines the flattened sound sets of its syls into a list.
-- The structure (onset-nucleus-coda) of the syls is lost in this
-- transformation.
sounds :: Sound.Word.Word -> [Sound]
sounds w = foldl1 (++) $ Syl.sounds <$> w

-- | stress provides the list of stress levels corresponding to each of its
-- syls.
stress :: Sound.Word.Word -> [Stress]
stress w = Syl.stress <$> w
