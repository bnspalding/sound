-- |
-- Module: Sound.GenAm
-- Description: General American English sound mapping
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.GenAm provides a mapping from a set of symbols ("Sound.Sound") to a
-- set of phonological features ("Sound.Feature"), based on the \'General
-- American English\' accent (see <https://en.wikipedia.org/wiki/General_American_English>).
--
-- The reasoning behind providing the GenAm mapping is twofold. First, it uses a
-- much more manageable subset of the IPA, which decreases the burden of
-- constructing the data and working with it. Second, different IPA sounds that
-- are not differentiated in the GenAm accent can be condensed, so that the
-- symbol set better corresponds to the author's intended pronunciations of
-- words when working with sounds.
--
-- As a mapping, it leaves open the possibility of reusing components of the
-- sound package with other mappings that can be constructed later. However,
-- there are certain portions of the package that are currently tied to GenAm in
-- ways that will require untangling in the future.
module Sound.GenAm
  ( sounds,
    features,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sound.Feature
import qualified Sound.GenAm.Sounds as GenAm
import Sound.Sound

-- | The set of sounds (symbols) that comprise the GenAm accent
sounds :: Set.Set Sound
sounds = Map.keysSet GenAm._sounds

-- | a map from the symbols of GenAm to their associated
-- "Sound.Feature".'FeatureSet'. The result is Nothing in cases where there is
-- no mapping for the given symbol.
features :: Sound -> Maybe FeatureSet
features s = Map.lookup s GenAm._sounds
-- TODO: move GenAm specific code from IPA over here to GenAm
