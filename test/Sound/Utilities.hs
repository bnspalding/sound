module Sound.Utilities (mkPhoneme) where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Sound.Phoneme

mkPhoneme :: T.Text -> Phoneme
mkPhoneme sym = Phoneme sym HashSet.empty
