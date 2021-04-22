{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Sound.Feature
-- Description: distinctive features for describing phonemes
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
--
--  == Distinctive Features
--  - what is a feature (as an abstraction)
--  - binary and unary features
--  - what assumptions/theories am I using?
--  - link to wiki pages/theory descriptions
--
--
--  == Autosegmental Features
--
--
--  == Feature Geometry
--  - write an explanation of feature geometry (what and why)
--
-- === Feature Geometry Diagram
-- @
-- [+\/-round]  [+\/-anterior][+\/-distib]  [+\/-high][+\/-low][+\/-back]  [+\/-ATR]
--      |                |    |                  \\   |   /             |
--   [labial]           [coronal]                 [dorsal]        [pharyngeal]
--       \\_ _ _ _ _ _ _ _ | _ _ _ _ _ _ _ _ _ _ _ _ | _  _ _ _ _ _ _ _/
--                                       |
--                                     PLACE
--                                       |
--                                   X segment
--                               (+\/- consonantal)
--                                (+\/- sonorant)
--         _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |_ _ _ _ _ _ _ _ _ _ _ _ _
--        |                  |           |             |          |
-- [+\/-continuant]    [+\/-strident]   [lateral]   [nasal]   [laryngeal]
--                                                           /    |    \\
--                                                        [SG]  [CG]  [+/-voice]
-- @
module Sound.Feature
  ( -- * Features
    BinaryFeature (..),
    Segment (..),
    RootFeatures (..),
    AutosegmentalFeature (..),
    Place (..),
    LabialFeature (..),
    CoronalFeature (..),
    DorsalFeature (..),
    PharyngealFeature (..),
    LaryngealFeature (..),
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
import qualified Data.Text as T
import GHC.Generics (Generic)

data BinaryFeature
  = Plus
  | Minus
  deriving (Eq, Show, Generic)

instance Hashable BinaryFeature

-- | A segment is a structured collection of phonological features used to
--  describe a 'Sound.Phoneme.Phoneme'.
--
--  All segments have a collection of 'RootFeatures' that are bound to the
--  segment. An 'AutosegmentalFeature' is more fluid, and only a subet of all
--  autosegmental features are specified for any segment. These features behave
--  differently from root features when a segment undergoes a phonological
--  transformation.
data Segment = Segment
  { rootFeatures :: RootFeatures,
    autosegmentalFeatures :: HashSet.HashSet AutosegmentalFeature,
    symbol :: T.Text
  }
  deriving (Eq, Show, Generic)

instance Hashable Segment

-- | Root Features describe all phonological segments.
--
-- These features are bound to a segment and do not exhibit autosegmental
-- behaviors.
data RootFeatures = RootFeatures
  { -- | Constriction of the vocal tract: consonants (+); vowels (-).
    consonantal :: BinaryFeature,
    -- | Use of vocal tract: nasals, liquids, vowels (+); obstruents (-).
    sonorant :: BinaryFeature
  }
  deriving (Eq, Show, Generic)

instance Hashable RootFeatures

-- | Autosegmental Features describe phonological segments in a variety of ways.
--
-- These features can be targetted during transformations at a scope beyond
-- individual segments (autonomously), hence the descriptor \'autosegmental\'.
--
-- Some features are dependent on the presence of other parent features,
-- resulting in a tree structure.
data AutosegmentalFeature
  = -- | Air passes through the nasal tract: \/n\/, \/m\/, \/ŋ\/.
    Nasal
  | -- | Air passes to the sides around the tongue: \/l\/, \/ɹ\/.
    Lateral
  | -- | High-amplitude, high-frequence fricatives: sibilants (+).
    Strident BinaryFeature
  | -- | Continuous vs stopped airflow: fricatives, approximants (+); stops (-).
    Continuant BinaryFeature
  | -- | Place of articulation within the mouth.
    PlaceNode (HashSet.HashSet Place)
  | -- | Contrasts and distinctions at the larynx: voicing distinctions.
    Laryngeal (HashSet.HashSet LaryngealFeature)
  deriving (Eq, Show, Generic)

instance Hashable AutosegmentalFeature

-- | Place describes a location of constriction/articulation within the mouth.
--
-- This feature group captures dependencies of features that only appear at
-- certain points of articulation in the mouth. It also permits transformations
-- to target place of articulation as a group of features.
--
-- The presence of Place with an empty set of child features is still
-- meaningful, and describes the place of articulation without any further
-- features specified.
data Place
  = -- | Articulation using the lips: \/p\/, \/m\/, vowel rounding.
    Labial (HashSet.HashSet LabialFeature)
  | -- | Articulation using the front of the tongue: \/t\/, \/s\/, \/n\/.
    Coronal (HashSet.HashSet CoronalFeature)
  | -- | Articulation using the body of the tongue: \/k\/, \/ŋ\/, vowel space.
    Dorsal (HashSet.HashSet DorsalFeature)
  | -- | Articulation using the root of the tongue: ATR.
    Pharyngeal (HashSet.HashSet PharyngealFeature)
  deriving (Eq, Show, Generic)

instance Hashable Place

-- | Features determined by behavior involving the lips.
data LabialFeature
  = -- | Vowel rounding.
    Round
  deriving (Eq, Show, Generic)

instance Hashable LabialFeature

-- | Features determined by behavior involving the front of the tongue.
data CoronalFeature
  = -- | Relation of the tongue to the alveolar ridge: dentals, alveolars (+).
    Anterior BinaryFeature
  | -- | tongue blade (laminal) vs tongue tip: \/ʃ\/, \/θ\/ (+); \/s\/ (-).
    Distrib BinaryFeature
  deriving (Eq, Show, Generic)

instance Hashable CoronalFeature

-- | Features determined by behavior involving the body of the tongue.
--
-- Vowel space is defined with both a [+/-high] and a [+/-low], following a
-- tradition of characterizing high vowels as (+high,-low), low vowels as
-- (-high,+low), and mid vowels as (-high,-low).
data DorsalFeature
  = -- | High tongue position: high vowels (+); mid and low vowels (-).
    High BinaryFeature
  | -- | Low tongue position: low vowels (+); mid and high vowels (-).
    Low BinaryFeature
  | -- | Tongue is not front: back and central vowels (+); front vowels (-).
    Back BinaryFeature
  deriving (Eq, Show, Generic)

instance Hashable DorsalFeature

-- | Features determined by the behavior at the root of the tongue
newtype PharyngealFeature
  = -- | Tongue root is forward. Doubles as [+/-tense]. \/i\/, \/u\/ (+).
    -- ATR should be undefined for low vowels.
    AdvancedTongueRoot BinaryFeature
  deriving (Eq, Show, Generic)

instance Hashable PharyngealFeature

-- | Features determined by the behavior of the vocal folds.
data LaryngealFeature
  = -- | Open vocal folds: aspirated segments.
    SpreadGlottis
  | -- | Constricted vocal folds: ejectives, glottal stops.
    ConstrictedGlottis
  | -- | voicing distinction: \/b\/, \/d\/, \/ɡ\/ (+); \/p\/, \/t\/, \/k\/ (-).
    Voice BinaryFeature
  deriving (Eq, Show, Generic)

instance Hashable LaryngealFeature

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
