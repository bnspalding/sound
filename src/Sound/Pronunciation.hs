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
module Sound.Pronunciation
  ( Pronunciation,
    makePronunciation,
  )
where

import qualified Data.Text as T
import Sound
import Sound.GenAm.IPA
import Sound.Syllabify

-- | Pronunciation is a more contextual naming for Sound.Word
type Pronunciation = Sound.Word

-- | makePronunciation converts a string of characters into a particular
-- pronunciation. This can produce errors if the symbols passed into the
-- function cannot be properly parsed. (And that's a good thing. The choice to
-- fail instead of adding the complexity of something like an Either is fine for
-- the sort of generative work that this library is being used for)
makePronunciation :: T.Text -> Pronunciation
makePronunciation = syllabify . textToIPASounds
