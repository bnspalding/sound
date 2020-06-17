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
import qualified Sound.Accents.GenAm.IPA as GenAm
import Sound.Syllabify

-- | Pronunciation is a more contextual naming for Sound.Word
type Pronunciation = Sound.Word

-- | makePronunciation converts a string of characters into a particular
-- pronunciation. It returns nothing if the string contains characters that
-- cannot be recognized as GenAm IPA symbols.
makePronunciation :: T.Text -> Maybe Pronunciation
makePronunciation = fmap syllabify . GenAm.textToIPASounds
