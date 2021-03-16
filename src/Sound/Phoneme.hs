-- |
-- Module: Sound.Phoneme
-- Description: basic unit of speech sound
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- A Phoneme is a structure representing the basic unit of speech sound in
-- the Sound library. This structure is used by an Accent to provide the set of
-- phonemes recognized by that accent.
module Sound.Phoneme (Phoneme (..)) where

import qualified Data.Text as T
import Sound.Feature

-- | A 'Phoneme' is a set of features with a corresponding symbolic
-- representation
data Phoneme = Phoneme
  { -- | a phoneme represents a particular set of features
    features :: FeatureSet,
    -- | a phoneme is represented by a particular symbol
    symbol :: T.Text
  }
  deriving (Eq, Show)
