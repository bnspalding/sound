-- |
-- Module: Sound.Accents.GenAm
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
module Sound.Accents.GenAm
  ( symbols,
    phoneme,
    phonemes,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Sound.Accents.GenAm.Sounds as GenAm
import Sound.Phoneme

-- | phoneme provides a constructor for General American English phonemes. Given
-- the IPA symbol for a phoneme, a structure with the symbol and associated
-- phonological feature set is returned.
--
-- Unrecognized symbols will return Nothing.
phoneme :: T.Text -> Maybe Phoneme
phoneme sym = Phoneme sym <$> HashMap.lookup sym GenAm.featureMap

-- | The set of sounds (symbols) that comprise the GenAm accent
symbols :: HashSet.HashSet T.Text
symbols = HashMap.keysSet GenAm.featureMap

-- | A list of all the phonemes in the GenAm accent
-- TODO: make this a set
phonemes :: HashSet.HashSet Phoneme
phonemes = HashSet.fromList $ uncurry Phoneme <$> HashMap.toList GenAm.featureMap
