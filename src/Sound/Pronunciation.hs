-- |
-- Module: N
-- Description: Short Description
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: Experimental
--
-- Longer description
module Sound.Pronunciation where

import Sound
import Sound.IPA
import Sound.Syllabify

type Pronunciation = Sound.Word

makePronunciation :: String -> Pronunciation
makePronunciation = syllabify . stringToIPASounds
