-- |
-- Module: Sound.Syl
-- Description: syllables
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Syl provides a data structure for representing syllables. Syllables are
-- broken into three groups of sounds: onset, nucleus, and coda. The nucleus and
-- coda together are commonly known as the rhyme. A syllable also is marked with
-- a level of stress, although this really only takes on meaning in relation to
-- the syllables around it.
module Sound.Syl where

import Sound.Sound
import Sound.Stress

-- | Syl describes a structured collection of sounds, what people commonly
-- distinguish as a unit out of which words are constructed.
data Syl
  = Syl
      { -- | The onset is the collection of sounds that begin a syllable
        onset :: [Sound],
        -- | The nucleus is the most sonorous sound at the center of a syllable.
        -- The nucleus is normally a vowel, and is normally length 1.
        nucleus :: [Sound],
        -- | The coda is the collection of sounds that follow the nucleus.
        coda :: [Sound],
        -- | stress marks the level of stress that is put on this syllable when
        -- it is spoken.
        stress :: Stress
      }
  deriving (Eq)

instance Show Syl where
  show syl = show (sounds syl)

-- | The rhyme is the nucleus and coda of a syllable together.
rhyme :: Syl -> [Sound]
rhyme syl = nucleus syl ++ coda syl

-- | sounds flattens a syllable into a single ordered list of sounds
sounds :: Syl -> [Sound]
sounds syl = onset syl ++ nucleus syl ++ coda syl
