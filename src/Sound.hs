-- |
-- Module: Sound
-- Description: top-level types
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound exposes top-level types for use. It's not particularly useful without the
-- addition of lower modules (which provide functions for operating on the types),
-- but sometimes all you need is the types.
module Sound
  ( module Sound.Sound,
    module Sound.Stress,
    module Sound.Syl,
    module Sound.Word,
  )
where

import Sound.Sound (Sound (..))
import Sound.Stress (Stress (..))
import Sound.Syl (Syl (..))
import Sound.Word (Word)

-- TODO: add hackage documentation to all files
