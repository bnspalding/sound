-- |
-- Module: Sound.NaturalClasses
-- Description: Predicates for natural classes of phonemes
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Natural Classes are sets of phonemes constructed using one or more
-- distinctive phonological feature. For example, the set of nasals is all
-- phonemes (in a language) that have the feature nasal, and the set of stops is
-- all phonemes that have the features -sonorant, -continuant.
module Sound.NaturalClasses
  ( -- * Natural Classes of Phonemes
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
  )
where

import Sound.Feature
import Sound.Phoneme

-- Natural Classes
--
-- UNFINISHED: get these natural classes set up. Abandon lists, because
-- everything in a list needs to be the same type. If sticking with lists, the
-- items need to all be type (Segment -> Bool). Perhaps use a helper function to
-- achieve this.
--
-- Once all the classes are set up, make sure that documentation is good, remove
-- generic and hashable references in phoneme, and then start fixing places
-- where phoneme is used farther up the chain.

-- | isStop reports whether or not a Phoneme describes a stop (-sonorant,
-- -continuant). See <https://en.wikipedia.org/wiki/Stop_consonant>
isStop :: Phoneme -> Bool
isStop =
  match
    [ bFeatureQuery (Just . getSonorant) Minus,
      bFeatureQuery getContinuant Minus
    ]

-- | isVoiced reports whether or not a Phoneme describes a voiced sound (+voiced).
-- See <https://en.wikipedia.org/wiki/Voice_(phonetics)>
isVoiced :: Phoneme -> Bool
isVoiced = match [bFeatureQuery getVoice Plus]

-- | isFricative reports whether or not a Phoneme describes a fricative
-- (+continuant, -sonorant). See <https://en.wikipedia.org/wiki/Fricative_consonant>
isFricative :: Phoneme -> Bool
isFricative =
  match
    [ bFeatureQuery getContinuant Plus,
      bFeatureQuery (Just . getSonorant) Minus
    ]

-- | isAffricate reports whether or not a Phoneme describes an affricate
-- (Disegment Stop Fricative). See <https://en.wikipedia.org/wiki/Affricate_consonant>
isAffricate :: Phoneme -> Bool
isAffricate (Monosegment _) = False
isAffricate (Disegment seg1 seg2) = onSeg seg1 isStop && onSeg seg2 isFricative

-- | isNasal reports whether or not a Phoneme describes a nasal sound (+nasal).
-- See <https://en.wikipedia.org/wiki/Nasal_consonant>
isNasal :: Phoneme -> Bool
isNasal = match [uFeatureQuery getNasal]

-- | isLateral reports whether or not a Phoneme describes a lateral sound (+lateral).
-- See <https://en.wikipedia.org/wiki/Lateral_consonant>
isLateral :: Phoneme -> Bool
isLateral = match [uFeatureQuery getLateral]

-- | isApproximant reports whether or not a Phoneme describes an approximant
-- (+continuant, +sonorant, -syllabic) See <https://en.wikipedia.org/wiki/Approximant_consonant>
isApproximant :: Phoneme -> Bool
isApproximant =
  match
    [ bFeatureQuery getContinuant Plus,
      bFeatureQuery (Just . getSonorant) Plus,
      bFeatureQuery (Just . getSyllabic) Minus
    ]

-- | isGlide reports whether or not a Phoneme describes a glide (-consonantal,
-- +syllabic). See <https://en.wikipedia.org/wiki/Semivowel>
isGlide :: Phoneme -> Bool
isGlide =
  match
    [ bFeatureQuery (Just . getConsonantal) Minus,
      bFeatureQuery (Just . getSyllabic) Minus
    ]

-- | isVowel reports whether or not a Phoneme describes a vowel (+syllabic)
-- See <https://en.wikipedia.org/wiki/Vowel>
isVowel :: Phoneme -> Bool
isVowel = match [bFeatureQuery (Just . getSyllabic) Plus]

-- | isHighVowel reports whether or not a Phoneme describes a high vowel
-- (+syllabic, +high). See <https://en.wikipedia.org/wiki/Vowel#Height>
isHighVowel :: Phoneme -> Bool
isHighVowel =
  match
    [ bFeatureQuery (Just . getSyllabic) Plus,
      bFeatureQuery getHigh Plus
    ]

-- | isMidVowel reports whether or not a Phoneme describes a high vowel
-- (+syllabic, -high, -low). See <https://en.wikipedia.org/wiki/Vowel#Height>
isMidVowel :: Phoneme -> Bool
isMidVowel =
  match
    [ bFeatureQuery (Just . getSyllabic) Plus,
      bFeatureQuery getHigh Minus,
      bFeatureQuery getLow Minus
    ]

-- | isLowVowel reports whether or not a Phoneme describes a high vowel
-- (+syllabic, +low). See <https://en.wikipedia.org/wiki/Vowel#Height>
isLowVowel :: Phoneme -> Bool
isLowVowel =
  match
    [ bFeatureQuery (Just . getSyllabic) Plus,
      bFeatureQuery getLow Plus
    ]

match :: [Segment -> Bool] -> Phoneme -> Bool
match = matchWith or

matchWith :: ([Bool] -> Bool) -> [Segment -> Bool] -> Phoneme -> Bool
matchWith merge queries p =
  let test seg = and $ fmap ($ seg) queries
   in case p of
        (Monosegment seg) -> test seg
        (Disegment seg1 seg2) -> merge $ test <$> [seg1, seg2]

onSeg :: Segment -> (Phoneme -> Bool) -> Bool
onSeg seg f = f (Monosegment seg)

bFeatureQuery :: (Segment -> Maybe BinaryFeature) -> BinaryFeature -> Segment -> Bool
bFeatureQuery accessor feat = (== Just feat) . accessor

uFeatureQuery :: (Segment -> Maybe UnaryFeature) -> Segment -> Bool
uFeatureQuery accessor seg =
  case accessor seg of
    Just Marked -> True
    Nothing -> False
