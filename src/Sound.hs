-- |
-- Module: Sound
-- Description: top-level types
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound exposes top-level types for use. It's not particularly useful without the
-- addition of lower modules (which provide functions for operating on the types),
-- but sometimes all you need is the types.
module Sound
  ( module Sound.Phoneme,
    module Sound.Stress,
    module Sound.Syllable,
    module Sound.Word,
  )
where

import Sound.Phoneme (Phoneme (..))
import Sound.Stress (Stress (..))
import Sound.Syllable (Syllable (..))
import Sound.Word (Word)
