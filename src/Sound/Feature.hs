-- |
-- Module: Sound.Feature
-- Description: distinctive features for describing phonemes
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Feature contains tools for representing and working with distincitve
-- phonological features, the points of contrast by which speech sounds are
-- identified in relation to each other.
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
--
-- >[+/-round]  [+/-anterior][+/-distrib]  [+/-high][+/-low][+/-back]  [+/-ATR]
-- >     |                |    |                  \   |   /              |
-- >  [labial]           [coronal]                 [dorsal]        [pharyngeal]
-- >      \ _ _ _ _ _ _ _ _ | _ _ _ _ _ _ _ _ _ _ _ _ | _  _ _ _ _ _ _ /
-- >                                      |
-- >                                    PLACE
-- >                                      |
-- >                                  X segment
-- >                              (+/- consonantal)
-- >                               (+/- syllabic)
-- >                               (+/- sonorant)
-- >        _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ | _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
-- >       |                  |           |             |         |      \
-- > [+/-continuant]  [+/-strident]  [laryngeal]  [lateral]   [nasal]  [rhotic]
-- >                                 /    |    \
-- >                              [SG]  [CG]  [+/-voice]
module Sound.Feature
  ( -- * Segment
    Segment (..),
    RootFeatures (..),

    -- * Binary Feature
    BinaryFeature (..),
    UnaryFeature (..),

    -- * Augosegmental Features and Structure
    AutosegmentalFeatures (..),
    Place (..),
    LabialFeatures (..),
    CoronalFeatures (..),
    DorsalFeatures (..),
    PharyngealFeatures (..),
    LaryngealFeatures (..),

    -- * Feature Accessors
    getConsonantal,
    getSonorant,
    getSyllabic,
    getStrident,
    getContinuant,
    getLateral,
    getNasal,
    getRhotic,
    getLaryngeal,
    getSpreadGlottis,
    getConstrictedGlottis,
    getVoice,
    getPlace,
    getLabial,
    getCoronal,
    getAnterior,
    getDistrib,
    getHigh,
    getLow,
    getBack,
    getAdvancedTongueRoot,
    getDorsal,
    getPharyngeal,
  )
where

import Control.Monad ((>=>))
import qualified Data.Text as T
import Prelude hiding (round)

-- | A Binary Feature describes a contrastive feature.
--
-- Both the markedness (+) and unmarkedness (-) of the feature can be used to
-- construct a natural class of sounds. This is different from a unary feature,
-- such as Nasal or Lateral, which are only meaningful as marked classes (-nasal
-- is not a utilized class of sounds).
--
-- When a feature is absent (neither marked nor unmarked), it means the
-- mechanical preconditions for the feature are not present. For example, vowel
-- space features such as [+\/-high], [+\/-low], [+\/-back] are not specified
-- for dorsal consonants.
data BinaryFeature
  = -- | The feature contrasts positively (it is notably there).
    Plus
  | -- | The feature contrasts negatively (it is notably not there).
    Minus
  deriving (Eq, Show)

data UnaryFeature = Marked
  deriving (Eq, Show)

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
    autosegmentalFeatures :: AutosegmentalFeatures,
    symbol :: T.Text
  }
  deriving (Eq, Show)

-- | Root Features describe all phonological segments.
--
-- These features are bound to a segment and do not exhibit autosegmental
-- behaviors.
data RootFeatures = RootFeatures
  { -- | Constriction of the vocal tract: consonants (+); vowels (-).
    consonantal :: BinaryFeature,
    -- | Use of vocal tract: nasals, liquids, vowels (+); obstruents (-).
    sonorant :: BinaryFeature,
    syllabic :: BinaryFeature
  }
  deriving (Eq, Show)

-- | Autosegmental Features describe phonological segments in a variety of ways.
--
-- These features can be targetted during transformations at a scope beyond
-- individual segments (autonomously), hence the descriptor \'autosegmental\'.
--
-- Some features are dependent on the presence of other parent features,
-- resulting in a tree structure.
data AutosegmentalFeatures = AutosegmentalFeatures
  { -- | Air passes through the nasal tract: \/n\/, \/m\/, \/ŋ\/.
    nasal :: Maybe UnaryFeature,
    -- | Air passes to the sides around the tongue: \/l\/, \/ɹ\/.
    lateral :: Maybe UnaryFeature,
    -- | Any of the different ways that rhoticity is marked.
    rhotic :: Maybe UnaryFeature,
    -- | High-amplitude, high-frequence fricatives: sibilants (+).
    strident :: Maybe BinaryFeature,
    -- | Continuous vs stopped airflow: fricatives, approximants (+); stops (-).
    continuant :: Maybe BinaryFeature,
    -- | Place of articulation within the mouth.
    place :: Place,
    -- | Contrasts and distinctions at the larynx: voicing distinctions.
    laryngeal :: Maybe LaryngealFeatures
  }
  deriving (Eq, Show)

-- | Place describes a location of constriction/articulation within the mouth.
--
-- This feature group captures dependencies of features that only appear at
-- certain points of articulation in the mouth. It also permits transformations
-- to target place of articulation as a group of features.
--
-- The presence of Place with an empty set of child features is still
-- meaningful, and describes the place of articulation without any further
-- features specified.
data Place = Place
  { -- | Articulation using the lips: \/p\/, \/m\/, vowel rounding.
    labial :: Maybe LabialFeatures,
    -- | Articulation using the front of the tongue: \/t\/, \/s\/, \/n\/.
    coronal :: Maybe CoronalFeatures,
    -- | Articulation using the body of the tongue: \/k\/, \/ŋ\/, vowel space.
    dorsal :: Maybe DorsalFeatures,
    -- | Articulation using the root of the tongue: ATR.
    pharyngeal :: Maybe PharyngealFeatures
  }
  deriving (Eq, Show)

-- | Features determined by behavior involving the lips.
newtype LabialFeatures = LabialFeatures
  { -- | Vowel rounding.
    round :: Maybe UnaryFeature
  }
  deriving (Eq, Show)

-- | Features determined by behavior involving the front of the tongue.
data CoronalFeatures = CoronalFeatures
  { -- | Relation of the tongue to the alveolar ridge: dentals, alveolars (+).
    anterior :: Maybe BinaryFeature,
    -- | tongue blade (laminal) vs tongue tip: \/ʃ\/, \/θ\/ (+); \/s\/ (-).
    distrib :: Maybe BinaryFeature
  }
  deriving (Eq, Show)

-- | Features determined by behavior involving the body of the tongue.
--
-- Vowel space is defined with both a [+/-high] and a [+/-low], following a
-- tradition of characterizing high vowels as (+high,-low), low vowels as
-- (-high,+low), and mid vowels as (-high,-low).
data DorsalFeatures = DorsalFeatures
  { -- | High tongue position: high vowels (+); mid and low vowels (-).
    high :: Maybe BinaryFeature,
    -- | Low tongue position: low vowels (+); mid and high vowels (-).
    low :: Maybe BinaryFeature,
    -- | Tongue is not front: back and central vowels (+); front vowels (-).
    back :: Maybe BinaryFeature
  }
  deriving (Eq, Show)

-- | Features determined by the behavior at the root of the tongue
newtype PharyngealFeatures = PharyngealFeatures
  { -- | Tongue root is forward. Doubles as [+/-tense]. \/i\/, \/u\/ (+).
    -- ATR should be undefined for low vowels.
    advancedTongueRoot :: Maybe BinaryFeature
  }
  deriving (Eq, Show)

-- | Features determined by the behavior of the vocal folds.
data LaryngealFeatures = LaryngealFeatures
  { -- | Open vocal folds: aspirated segments.
    spreadGlottis :: Maybe UnaryFeature,
    -- | Constricted vocal folds: ejectives, glottal stops.
    constrictedGlottis :: Maybe UnaryFeature,
    -- | voicing distinction: \/b\/, \/d\/, \/ɡ\/ (+); \/p\/, \/t\/, \/k\/ (-).
    voice :: Maybe BinaryFeature
  }
  deriving (Eq, Show)

-- Feature Accessors

-- | Direct accessor for 'consonantal' feature on a segment.
getConsonantal :: Segment -> BinaryFeature
getConsonantal = consonantal . rootFeatures

-- | Direct accessor for 'sonorant' feature on a segment.
getSonorant :: Segment -> BinaryFeature
getSonorant = sonorant . rootFeatures

-- | Direct accessor for 'syllabic' feature on a segment.
getSyllabic :: Segment -> BinaryFeature
getSyllabic = syllabic . rootFeatures

-- | Direct accessor for 'strident' feature on a segment.
getStrident :: Segment -> Maybe BinaryFeature
getStrident = strident . autosegmentalFeatures

-- | Direct accessor for 'continuant' feature on a segment.
getContinuant :: Segment -> Maybe BinaryFeature
getContinuant = continuant . autosegmentalFeatures

-- | Direct accessor for 'lateral' feature on a segment.
getLateral :: Segment -> Maybe UnaryFeature
getLateral = lateral . autosegmentalFeatures

-- | Direct accessor for 'nasal' feature on a segment.
getNasal :: Segment -> Maybe UnaryFeature
getNasal = nasal . autosegmentalFeatures

-- | Direct accessor for 'rhotic' feature on a segment.
getRhotic :: Segment -> Maybe UnaryFeature
getRhotic = rhotic . autosegmentalFeatures

-- | Direct accessor for 'laryngeal' feature on a segment.
getLaryngeal :: Segment -> Maybe LaryngealFeatures
getLaryngeal = laryngeal . autosegmentalFeatures

-- | Direct accessor for 'spreadGlottis' feature on a segment.
getSpreadGlottis :: Segment -> Maybe UnaryFeature
getSpreadGlottis = getLaryngeal >=> spreadGlottis

-- | Direct accessor for 'constrictedGlottis' feature on a segment.
getConstrictedGlottis :: Segment -> Maybe UnaryFeature
getConstrictedGlottis = getLaryngeal >=> constrictedGlottis

-- | Direct accessor for 'voice' feature on a segment.
getVoice :: Segment -> Maybe BinaryFeature
getVoice = getLaryngeal >=> voice

-- | Direct accessor for 'place' feature on a segment.
getPlace :: Segment -> Place
getPlace = place . autosegmentalFeatures

-- | Direct accessor for 'labial' feature on a segment.
getLabial :: Segment -> Maybe LabialFeatures
getLabial = labial . getPlace

-- | Direct accessor for 'coronal' feature on a segment.
getCoronal :: Segment -> Maybe CoronalFeatures
getCoronal = coronal . getPlace

-- | Direct accessor for 'anterior' feature on a segment.
getAnterior :: Segment -> Maybe BinaryFeature
getAnterior = getCoronal >=> anterior

-- | Direct accessor for 'distrib' feature on a segment.
getDistrib :: Segment -> Maybe BinaryFeature
getDistrib = getCoronal >=> distrib

-- | Direct accessor for 'dorsal' feature on a segment.
getDorsal :: Segment -> Maybe DorsalFeatures
getDorsal = dorsal . getPlace

-- | Direct accessor for 'high' feature on a segment.
getHigh :: Segment -> Maybe BinaryFeature
getHigh = getDorsal >=> high

-- | Direct accessor for 'low' feature on a segment.
getLow :: Segment -> Maybe BinaryFeature
getLow = getDorsal >=> low

-- | Direct accessor for 'back' feature on a segment.
getBack :: Segment -> Maybe BinaryFeature
getBack = getDorsal >=> back

-- | Direct accessor for 'pharyngeal' feature on a segment.
getPharyngeal :: Segment -> Maybe PharyngealFeatures
getPharyngeal = pharyngeal . getPlace

-- | Direct accessor for 'advancedTongueRoot' feature on a segment.
getAdvancedTongueRoot :: Segment -> Maybe BinaryFeature
getAdvancedTongueRoot = getPharyngeal >=> advancedTongueRoot
