-- |
-- Module: Sound.Pronunciation
-- Description: as pronunciations
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Pronunciation pulls together the specifically relevant pieces of Sound
-- for thinking about pronunciations. It provides some additional tools, such as
-- 'makePronunciation', for converting into Sound and working with
-- pronunciations.
--
-- Some elements, like the conversion into sounds done by 'makePronunciation'
-- are based on the mappings from "Sound.GenAm".
module Sound.Pronunciation where

import Sound
import Sound.IPA
import Sound.Syllabify

-- | Pronunciation is a more contextual naming for Sound.Word
type Pronunciation = Sound.Word

-- | makePronunciation converts a string of characters into a particular
-- pronunciation. This can produce errors if the symbols passed into the
-- function cannot be properly parsed.
makePronunciation :: String -> Pronunciation
makePronunciation = syllabify . stringToIPASounds
-- TODO: change the output type to Either, to better represent capacity for
-- errors
