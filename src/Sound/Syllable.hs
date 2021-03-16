-- |
-- Module: Sound.Syl
-- Description: syllables
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Syl provides a data structure for representing syllables. Syllables are
-- broken into three groups of phonemes: onset, nucleus, and coda. The nucleus
-- and coda together are known as the rhyme. A syllable may also be marked
-- with a level of stress. It is recommended to interpret stress relatively,
-- only in relation to other syllables in the same word (lexical stress).
module Sound.Syllable where

import qualified Data.Text as T
import Sound.Phoneme (Phoneme (..))
import qualified Sound.Phoneme as Phoneme (symbol)
import Sound.Stress (Stress)
import qualified Sound.Stress as Stress (symbol)

-- | Syllable describes a structured collection of phonemes, the distinguished units
-- out of which words are constructed.
data Syllable = Syllable
  { -- | The onset is the collection of sounds that begin a syllable
    onset :: [Phoneme],
    -- | The nucleus is the most sonorous sound at the center of a syllable.
    nucleus :: Phoneme,
    -- | The coda is the collection of sounds that follow the nucleus.
    coda :: [Phoneme],
    -- | stress marks the relative level of stress that is put on this syllable
    -- when it is spoken.
    stress :: Maybe Stress
  }
  deriving (Eq, Show)

-- | The rhyme is the nucleus and coda of a syllable together.
rhyme :: Syllable -> [Phoneme]
rhyme syl = nucleus syl : coda syl

-- | phonemes flattens a syllable into a single ordered list of phonemes
phonemes :: Syllable -> [Phoneme]
phonemes syl = onset syl ++ nucleus syl : coda syl

-- | symbols returns the symbolic representation of a syllable's phonemes as a
-- single Text object. This includes the stress symbol for the syl if it is
-- stressed.
symbols :: Syllable -> T.Text
symbols syl = T.concat $ stressSymbol : soundSymbols
  where
    soundSymbols = fmap Phoneme.symbol . phonemes $ syl
    stressSymbol = maybe T.empty T.singleton $ Stress.symbol =<< stress syl
