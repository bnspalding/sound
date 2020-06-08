-- |
-- Module: Sound.Sound
-- Description: sound type
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Sound furnishes a Sound type. This type is intended to be used with a
-- mapping like "Sound.GenAm" which maps sounds to sets of "Sound.Feature"s.
module Sound.Sound
  ( Sound (..),
    symbol,
  )
where

import qualified Data.Text as T

-- | A Sound is a symbol. When connected to an appropriate mapping, it
-- represents the set of phonological features (see "Sound.Feature") that
-- describe a phoneme for a particular language.
newtype Sound
  = Sound T.Text
  deriving (Eq, Ord)

instance Show Sound where
  show (Sound s) = show s

-- | Symbol returns the symbol associated with a sound
--
-- A symbol may be a single character (most IPA symbols), but it may also be a
-- symbol constructed from multiple characters (t͡ʃ, diphthongs, diacritics in
-- IPA, many ARPAbet or X-SAMPA symbols).
symbol :: Sound -> T.Text
symbol (Sound s) = s
