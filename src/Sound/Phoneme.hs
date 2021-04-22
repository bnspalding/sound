{-# LANGUAGE DeriveGeneric #-}

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
module Sound.Phoneme (
  Phoneme (..),
  symbol,
  features
  ) where

import Data.Hashable
import GHC.Generics (Generic)
import Sound.Feature (Segment, FeatureSet)
import qualified Sound.Feature as Feature
import qualified Data.Text as T

-- | A Phoneme is a unit of speech sound.
--
-- Most phonemes are monosegments, like 'ɪ' or 't'. However, there are also
-- instances where a single phoneme behaves like a sequence of two phonological
-- segments (a disegment), as in the cases of diphthongs (\'a͡ɪ\') or affricates
-- (\'t͡ʃ\'). This representation does away with the need for a \'delrel\' feature
-- on segments.
data Phoneme
  = -- | A phoneme with a single phonological segment
    Monosegment Segment
  | -- | A phoneme comprised of an ordered sequence of two segments
    Disegment Segment Segment
  deriving (Eq, Show, Generic)

instance Hashable Phoneme

symbol :: Phoneme -> T.Text
symbol (Monosegment seg) = Feature.symbol seg
symbol (Disegment seg1 seg2) = Feature.symbol seg1 <> Feature.symbol seg2

features :: Phoneme -> FeatureSet
features = undefined
