{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Sound.Accents.GenAm.Sounds
-- Description: GenAm data
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- GenAm.Phonemes contains the actual mappings from symbols to FeatureSets.
module Sound.Accents.GenAm.Sounds where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Sound.Feature

-- map from IPA symbols to phonological features
featureMap :: Map.Map T.Text FeatureSet
featureMap = consonants <> vowels

consonants :: Map.Map T.Text FeatureSet
consonants =
  Map.fromList
    [ ( "m", -- vd bilabial nasal
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            PLUS_SONORANT,
            MINUS_CONTINUANT,
            NASAL,
            PLUS_VOICE,
            LABIAL
          ]
      ),
      ( "n", -- vd alveolar nasal
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            PLUS_SONORANT,
            MINUS_CONTINUANT,
            NASAL,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            MINUS_DISTRIB
          ]
      ),
      ( "ŋ", -- 014B vd velar nasal
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            PLUS_SONORANT,
            MINUS_CONTINUANT,
            NASAL,
            PLUS_VOICE,
            DORSAL
          ]
      ),
      ( "p", -- 0070 vl bilabial stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            MINUS_VOICE,
            LABIAL
          ]
      ),
      ( "b", -- 0062 vd bilabial stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            PLUS_VOICE,
            LABIAL
          ]
      ),
      ( "t", -- 0074 vl alveolar stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            MINUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            MINUS_DISTRIB
          ]
      ),
      ( "d", -- 0064 vd alveolar stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            MINUS_DISTRIB
          ]
      ),
      ( "k", -- 006B vl velar stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            MINUS_VOICE,
            DORSAL
          ]
      ),
      ( "ɡ", -- 0261 vd velar stop
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            PLUS_VOICE,
            DORSAL
          ]
      ),
      ( "t͡ʃ", -- 0074 0361 0283 vl postalveolar affricate
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            DELREL,
            MINUS_VOICE,
            CORONAL,
            MINUS_ANTERIOR,
            MINUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "d͡ʒ", -- 0064 0361 0292, vd postalveolar affricate
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            DELREL,
            PLUS_VOICE,
            CORONAL,
            MINUS_ANTERIOR,
            MINUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "f", -- 0066, vl labiodental fricative,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            LABIAL,
            PLUS_STRIDENT
          ]
      ),
      ( "v", -- 0076, vd labiodental fricative
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            LABIAL,
            PLUS_STRIDENT
          ]
      ),
      ( "θ", -- 03B8, vl dental fricative,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            PLUS_DISTRIB,
            MINUS_STRIDENT
          ]
      ),
      ( "ð", -- 00F0, vd dental fricative
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            PLUS_DISTRIB,
            MINUS_STRIDENT
          ]
      ),
      ( "s", -- 0073, vl alveolar fricative,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            MINUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "z", -- 007A, vd alveolar fricative
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            MINUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "ʃ", -- 0283, vl postalveolar fricative,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            CORONAL,
            MINUS_ANTERIOR,
            PLUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "ʒ", -- 0292, vd postalveolar fricative
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            CORONAL,
            MINUS_ANTERIOR,
            PLUS_DISTRIB,
            PLUS_STRIDENT
          ]
      ),
      ( "h", -- 0068, vl glottal fricative,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            LARYNGEAL,
            MINUS_STRIDENT
          ]
      ),
      ( "l", -- 006C, vd alveolar lateral approximant,
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            LATERAL,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            PLUS_DISTRIB
          ]
      ),
      ( "ɹ", -- 0279, vd alveolar approximant
        Set.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            CORONAL,
            PLUS_ANTERIOR,
            PLUS_DISTRIB
          ]
      ),
      ( "j", -- 006A, vd palatal approximant,
        Set.fromList
          [ MINUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            DORSAL
          ]
      ),
      ( "ʍ", -- 028D, vl labial-velar co-articulated approximant,
        Set.fromList
          [ MINUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_VOICE,
            LABIAL,
            DORSAL
          ]
      ),
      ( "w", -- 0077, vd labial-velar co-articulated approximant
        Set.fromList
          [ MINUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_VOICE,
            LABIAL,
            DORSAL
          ]
      )
    ]

vowels :: Map.Map T.Text FeatureSet
vowels =
  Map.fromList
    [ ( "i", -- , 0069, close front unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            PLUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            PLUS_ATR
          ]
      ),
      ( "ɪ", -- , 026A, near-close front unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            PLUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "ɛ", -- , 025B, open-mid front unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            PLUS_ATR
          ]
      ),
      ( "æ", --, 00E6, near-open front unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            PLUS_LOW,
            MINUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "ə", -- , 0259, mid central unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "ʌ", -- , 028C, open-mid back unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "ɑ", -- , 0251, open back unrounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            PLUS_LOW,
            PLUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "u", -- , 0075, close back rounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_ROUND,
            PLUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            PLUS_ATR
          ]
      ),
      ( "ʊ", -- , 028A, near-close back rounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_ROUND,
            PLUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "ɔ", -- , 0254, open-mid back rounded,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            MINUS_ATR
          ]
      ),
      ( "e͡ɪ", -- , 0065 0361 026A,
        Set.fromList -- diphthong, closing mid-to-high narrow front
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            PLUS_ATR,
            MINUS_WIDE
          ]
      ),
      ( "a͡ɪ", -- , 0061 0361 026A, diphthong closing low-to-high wide,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            MINUS_ATR,
            PLUS_WIDE
          ]
      ),
      ( "a͡ʊ", -- , 0061 0361 028A, diphthong closing low-to-high backward front-to-back wide,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_ROUND,
            MINUS_HIGH,
            PLUS_LOW,
            MINUS_BACK,
            MINUS_ATR,
            PLUS_WIDE
          ]
      ),
      ( "o͡ʊ", -- , 006F 0361 028A, diphthong closing mid-to-high narrow back,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            PLUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            PLUS_ATR,
            MINUS_WIDE
          ]
      ),
      ( "ɔ͡ɪ", -- , 0254 0361 026A, diphthong closing mid-to-high forward back-to-front wide,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            PLUS_BACK,
            MINUS_ATR,
            PLUS_WIDE
          ]
      ),
      ( "ɜ˞", -- , 025C 02DE, mid central rhotic stressed,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            MINUS_ATR,
            RHOTIC,
            PLUS_STRESSED
          ]
      ),
      ( "ə˞", -- , 0259 02DE, mid central rhotic unstressed,
        Set.fromList
          [ PLUS_SYLLABIC,
            MINUS_CONSONANTAL,
            PLUS_SONORANT,
            PLUS_CONTINUANT,
            MINUS_ROUND,
            MINUS_HIGH,
            MINUS_LOW,
            MINUS_BACK,
            MINUS_ATR,
            RHOTIC,
            MINUS_STRESSED
          ]
      )
    ]
