{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text as T
import Sound.Sound
import Sound.Stress
import Sound.Syl as Syl
import Prelude hiding (Word)

-- | a word is a list of syls. It represents a spoken word, or perhaps the
-- pronunciation that would correspond to a written word.
type Word = [Syl]

-- | sounds combines the flattened sound sets of its syls into a list.
-- The structure (onset-nucleus-coda) of the syls is lost in this
-- transformation.
sounds :: Word -> [Sound]
sounds w = foldl1 (++) $ Syl.sounds <$> w

-- | stress provides the list of stress levels corresponding to each of its
-- syls.
stress :: Word -> [Stress]
stress = fmap Syl.stress

-- | symbols returns a textual representation of a syllabized word, as a single
-- text object
symbols :: Word -> T.Text
symbols syls = cleanBreaks (T.intercalate "." (Syl.symbols <$> syls))
  where
    cleanBreaks :: T.Text -> T.Text
    cleanBreaks = T.replace ".ˈ" "ˈ" . T.replace ".ˌ" "ˌ"
