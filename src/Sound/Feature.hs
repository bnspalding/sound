{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Sound.Feature
-- Description: phonological features
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Feature contains tools for representing and working with distincitve
-- phonological features, the \'particles\' of which sounds are constructed. Any
-- sound will have an unordered set of features associated with it, a
-- 'FeatureSet'. In addition to 'Feature's, some common classes of sounds
-- (stops, fricatives, glides, high/low vowels) are defined in terms of features.
--
-- There does not appear to be strong consensus on the precise set of features
-- that are used to describe sounds, so this module is equal parts art and
-- science in its construction of a \'useful\' set of features, tuned largely
-- through the author's use of the module in his own context of poetry.
module Sound.Feature
  ( -- * Features
    Feature (..),
    FeatureSet,

    -- * Sound Classes from FeatureSets
    isVoiced,
    isStop,
    isFricative,
    isAffricate,
    isNasal,
    isLateral,
    isApproximant,
    isGlide,
    isVowel,
    isHighVowel,
    isMidVowel,
    isLowVowel,

    -- * Set Operations on FeatureSets

    -- | A 'FeatureSet' is just a Set, so any operation from "Data.Set" can be
    -- applied to a FeatureSet.
    featureSet,
    contains,
    contains1,
    featuresOrEmpty,
  )
where

import Data.HashSet as HashSet
import Data.Hashable
import GHC.Generics (Generic)

-- | contains reports whether a FeatureSet is contained within another
-- FeatureSet ("Data.Set".'Set.isSubsetOf')
contains :: FeatureSet -> FeatureSet -> Bool
contains = HashSet.isSubsetOf

-- | contains1 reports whether a single Feature is contained within a
-- FeatureSet ("Data.Set".'Set.member')
contains1 :: Feature -> FeatureSet -> Bool
contains1 = HashSet.member

-- | featuresOrEmpty returns either Just the FeatureSet contained in the Maybe,
-- or an empty set.
featuresOrEmpty :: Maybe FeatureSet -> FeatureSet
featuresOrEmpty (Just fs) = fs
featuresOrEmpty Nothing = HashSet.empty

-- | A Feature is a basic particle or building block out of which a
-- sound is described. See <https://en.wikipedia.org/wiki/Distinctive_feature>
-- as a starting point for more information.
data Feature
  = -- | +syllabic sounds can form the nucleus of a syllable
    PLUS_SYLLABIC
  | MINUS_SYLLABIC
  | -- | vocal tract constriction
    PLUS_CONSONANTAL
  | MINUS_CONSONANTAL
  | -- | pressure behind oral constriction
    PLUS_SONORANT
  | MINUS_SONORANT
  | -- | complete closure of oral cavity
    PLUS_CONTINUANT
  | MINUS_CONTINUANT
  | -- | articulated with open velum
    NASAL
  | -- | vocal tract closed at center, open at sides
    LATERAL
  | -- | delayed release (affricates)
    DELREL
  | -- | sounds that have vibrating vocal folds
    PLUS_VOICE
  | MINUS_VOICE
  | -- | spread glottis (large glottal opening gesture)
    SG
  | -- | constricted glottis (constricted vocal folds)
    CG
  | -- | constriction at lips
    LABIAL
  | -- | constriction made with the tongue front
    CORONAL
  | -- | constriction made with the tongue body
    DORSAL
  | -- | constriction made with tongue root
    PHARYNGEAL
  | -- | constriction made at the glottis
    LARYNGEAL
  | -- | tongue at front of alveolar ridge
    PLUS_ANTERIOR
  | -- | tongue behind alveolar ridge
    MINUS_ANTERIOR
  | -- | relatively long constriction
    PLUS_DISTRIB
  | MINUS_DISTRIB
  | -- | high-amplitude, high-pitched fricative
    PLUS_STRIDENT
  | MINUS_STRIDENT
  | -- | vowels: pursing of the lips
    PLUS_ROUND
  | MINUS_ROUND
  | -- | vowels: tongue is raised above neutral
    PLUS_HIGH
  | MINUS_HIGH
  | -- | vowels: tongue is lowered below neutral
    PLUS_LOW
  | MINUS_LOW
  | -- | vowels: tongue is moved back from neutral
    PLUS_BACK
  | MINUS_BACK
  | -- | vowels: tongue root is pulled forward
    PLUS_ATR
  | MINUS_ATR
  | -- | diphthongs: wide vs narrow
    PLUS_WIDE
  | MINUS_WIDE
  | -- | r colored vowels
    RHOTIC
  | -- | differentiating between rhotic ə and ɜ (tenuous)
    PLUS_STRESSED
  | MINUS_STRESSED
  deriving (Eq, Show, Generic)

instance Hashable Feature

-- | A FeatureSet is a "Data.Set".'Set.Set' of Features
type FeatureSet = HashSet Feature

-- | featureSet constructs a FeatureSet from a list of Features
-- ("Data.Set".'Set.fromList')
featureSet :: [Feature] -> FeatureSet
featureSet = HashSet.fromList

-- | isStop reports whether or not a FeatureSet describes a stop (-sonorant,
-- -continuant, -delrel). See <https://en.wikipedia.org/wiki/Stop_consonant>
isStop :: FeatureSet -> Bool
isStop =
  ((&&) . contains (featureSet [MINUS_SONORANT, MINUS_CONTINUANT]))
    <*> (not . contains1 DELREL)

-- | isVoiced reports whether or not a FeatureSet describes a voiced sound (+voiced).
-- See <https://en.wikipedia.org/wiki/Voice_(phonetics)>
isVoiced :: FeatureSet -> Bool
isVoiced = contains1 PLUS_VOICE

-- | isFricative reports whether or not a FeatureSet describes a fricative
-- (+continuant, -sonorant). See <https://en.wikipedia.org/wiki/Fricative_consonant>
isFricative :: FeatureSet -> Bool
isFricative = contains (featureSet [PLUS_CONTINUANT, MINUS_SONORANT])

-- | isAffricate reports whether or not a FeatureSet describes an affricate (+delrel)
-- See <https://en.wikipedia.org/wiki/Affricate_consonant>
isAffricate :: FeatureSet -> Bool
isAffricate = contains1 DELREL

-- | isNasal reports whether or not a FeatureSet describes a nasal sound (+nasal).
-- See <https://en.wikipedia.org/wiki/Nasal_consonant>
isNasal :: FeatureSet -> Bool
isNasal = contains1 NASAL

-- | isLateral reports whether or not a FeatureSet describes a lateral sound (+lateral).
-- See <https://en.wikipedia.org/wiki/Lateral_consonant>
isLateral :: FeatureSet -> Bool
isLateral = contains1 LATERAL

-- | isApproximant reports whether or not a FeatureSet describes an approximant
-- (+continuant, +sonorant, -syllabic) See <https://en.wikipedia.org/wiki/Approximant_consonant>
isApproximant :: FeatureSet -> Bool
isApproximant =
  contains (featureSet [PLUS_CONTINUANT, PLUS_SONORANT, MINUS_SYLLABIC])

-- | isGlide reports whether or not a FeatureSet describes a glide (-consonantal,
-- +syllabic). See <https://en.wikipedia.org/wiki/Semivowel>
isGlide :: FeatureSet -> Bool
isGlide = contains (featureSet [MINUS_CONSONANTAL, MINUS_SYLLABIC])

-- | isVowel reports whether or not a FeatureSet describes a vowel (+syllabic)
-- See <https://en.wikipedia.org/wiki/Vowel>
isVowel :: FeatureSet -> Bool
isVowel = contains1 PLUS_SYLLABIC

-- | isHighVowel reports whether or not a FeatureSet describes a high vowel
-- (+syllabic, +high). See <https://en.wikipedia.org/wiki/Vowel#Height>
isHighVowel :: FeatureSet -> Bool
isHighVowel = contains (featureSet [PLUS_SYLLABIC, PLUS_HIGH])

-- | isMidVowel reports whether or not a FeatureSet describes a high vowel
-- (+syllabic, -high, -low). See <https://en.wikipedia.org/wiki/Vowel#Height>
isMidVowel :: FeatureSet -> Bool
isMidVowel = contains (featureSet [PLUS_SYLLABIC, MINUS_HIGH, MINUS_LOW])

-- | isLowVowel reports whether or not a FeatureSet describes a high vowel
-- (+syllabic, +low). See <https://en.wikipedia.org/wiki/Vowel#Height>
isLowVowel :: FeatureSet -> Bool
isLowVowel = contains (featureSet [PLUS_SYLLABIC, PLUS_LOW])
