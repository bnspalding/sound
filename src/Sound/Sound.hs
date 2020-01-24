-- |
-- Module: Sound.Sound
-- Description: sound type
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Sound furnishes a Sound type. This type is intended to be used with a
-- mapping like "Sound.GenAm" which maps sounds to sets of "Sound.Feature"s.
module Sound.Sound where

-- | A Sound is a symbol. When connected to an appropriate mapping, it
-- represents the set of phonological features (see "Sound.Feature") that
-- describe a phoneme for a particular language.
newtype Sound
  = Sound String
  deriving (Eq, Show, Ord)
-- TODO: change out Strings for T.Text across package
