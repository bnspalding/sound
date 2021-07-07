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

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Sound.Accents.Builders.Consonants
import qualified Sound.Accents.Builders.Consonants as C (rhotic)
import Sound.Accents.Builders.Vowels
import qualified Sound.Accents.Builders.Vowels as V (rhotic)
import Sound.Phoneme hiding (symbol)
import Prelude hiding (round)

-- map from IPA symbols to phonological features
featureMap :: HashMap.HashMap T.Text Phoneme
featureMap = consonants <> vowels

consonants :: HashMap.HashMap T.Text Phoneme
consonants =
  HashMap.fromList
    [ ( "m", -- vd bilabial nasal
        Monosegment $ (vd . bilabial . nasal) "m"
      ),
      ( "n", -- vd alveolar nasal
        Monosegment $ (vd . alveolar . nasal) "n"
      ),
      ( "ŋ", -- 014B vd velar nasal
        Monosegment $ (vd . velar . nasal) "ŋ"
      ),
      ( "p", -- 0070 vl bilabial stop
        Monosegment $ (vl . bilabial . stop) "p"
      ),
      ( "b", -- 0062 vd bilabial stop
        Monosegment $ (vd . bilabial . stop) "b"
      ),
      ( "t", -- 0074 vl alveolar stop
        Monosegment $ (vl . alveolar . stop) "t"
      ),
      ( "d", -- 0064 vd alveolar stop
        Monosegment $ (vd . alveolar . stop) "d"
      ),
      ( "k", -- 006B vl velar stop
        Monosegment $ (vl . velar . stop) "k"
      ),
      ( "ɡ", -- 0261 vd velar stop
        Monosegment $ (vd . velar . stop) "ɡ"
      ),
      ( "t͡ʃ", -- 0074 0361 0283 vl postalveolar affricate
        Disegment
          ((vl . alveolar . stop) "t")
          ((vl . postalveolar . distrib . sibilant . fricative) "ʃ")
      ),
      ( "d͡ʒ", -- 0064 0361 0292, vd postalveolar affricate
        Disegment
          ((vd . alveolar . stop) "d")
          ((vd . postalveolar . distrib . sibilant . fricative) "ʒ")
      ),
      ( "f", -- 0066, vl labiodental fricative,
        Monosegment $ (vl . labiodental . sibilant . fricative) "f"
      ),
      ( "v", -- 0076, vd labiodental fricative
        Monosegment $ (vd . labiodental . sibilant . fricative) "v"
      ),
      ( "θ", -- 03B8, vl dental fricative,
        Monosegment $ (vl . dental . distrib . fricative) "θ"
      ),
      ( "ð", -- 00F0, vd dental fricative
        Monosegment $ (vd . dental . distrib . fricative) "ð"
      ),
      ( "s", -- 0073, vl alveolar fricative,
        Monosegment $ (vl . alveolar . sibilant . fricative) "s"
      ),
      ( "z", -- 007A, vd alveolar fricative
        Monosegment $ (vd . alveolar . sibilant . fricative) "z"
      ),
      ( "ʃ", -- 0283, vl postalveolar fricative,
        Monosegment $ (vl . postalveolar . distrib . sibilant . fricative) "ʃ"
      ),
      ( "ʒ", -- 0292, vd postalveolar fricative
        Monosegment $ (vd . postalveolar . distrib . sibilant . fricative) "ʒ"
      ),
      ( "h", -- 0068, vl glottal fricative,
        Monosegment $ (vl . glottal . fricative) "h"
      ),
      ( "l", -- 006C, vd alveolar lateral approximant,
        Monosegment $ (vd . alveolar . lateral . distrib . approximant) "l"
      ),
      ( "ɹ", -- 0279, vd alveolar approximant
        Monosegment $ (vd . alveolar . distrib . C.rhotic . approximant) "ɹ"
      ),
      ( "j", -- 006A, vd palatal approximant,
        Monosegment $ (vd . palatal . glide) "j"
      ),
      ( "ʍ", -- 028D, vl labial-velar co-articulated approximant,
        Monosegment $ (vl . bilabial . velar . glide) "ʍ"
      ),
      ( "w", -- 0077, vd labial-velar co-articulated approximant
        Monosegment $ (vd . bilabial . velar . glide) "w"
      )
    ]

vowels :: HashMap.HashMap T.Text Phoneme
vowels =
  HashMap.fromList
    [ ( "i", -- , 0069, close front unrounded,
        Monosegment $ (high . front . tense . unrounded . vowel) "i"
      ),
      ( "ɪ", -- , 026A, near-close front unrounded,
        Monosegment $ (high . front . unrounded . vowel) "ɪ"
      ),
      ( "ɛ", -- , 025B, open-mid front unrounded,
        Monosegment $ (mid . front . tense . unrounded . vowel) "ɛ"
      ),
      ( "æ", --, 00E6, near-open front unrounded,
        Monosegment $ (low . front . unrounded . vowel) "æ"
      ),
      ( "ə", -- , 0259, mid central unrounded,
        Monosegment $ (mid . front . unrounded . vowel) "ə"
      ),
      ( "ʌ", -- , 028C, open-mid back unrounded,
        Monosegment $ (mid . back . unrounded . vowel) "ʌ"
      ),
      ( "ɑ", -- , 0251, open back unrounded,
        Monosegment $ (low . back . unrounded . vowel) "ɑ"
      ),
      ( "u", -- , 0075, close back rounded,
        Monosegment $ (high . back . rounded . tense . vowel) "u"
      ),
      ( "ʊ", -- , 028A, near-close back rounded,
        Monosegment $ (high . back . rounded . vowel) "ʊ"
      ),
      ( "ɔ", -- , 0254, open-mid back rounded,
        Monosegment $ (mid . back . rounded . vowel) "ɔ"
      ),
      ( "e͡ɪ", -- , 0065 0361 026A,
        Disegment
          ((mid . front . tense . unrounded . vowel) "e")
          ((high . front . unrounded . vowel) "ɪ")
      ),
      ( "a͡ɪ", -- , 0061 0361 026A, diphthong closing low-to-high wide,
        Disegment
          ((low . front . unrounded . vowel) "a")
          ((high . front . unrounded . vowel) "ɪ")
      ),
      ( "a͡ʊ", -- , 0061 0361 028A, diphthong closing low-to-high backward front-to-back wide,
        Disegment
          ((low . front . unrounded . vowel) "a")
          ((high . back . rounded . vowel) "ʊ")
      ),
      ( "o͡ʊ", -- , 006F 0361 028A, diphthong closing mid-to-high narrow back,
        Disegment
          ((mid . back . tense . rounded . vowel) "o")
          ((high . back . rounded . vowel) "ʊ")
      ),
      ( "ɔ͡ɪ", -- , 0254 0361 026A, diphthong closing mid-to-high forward back-to-front wide,
        Disegment
          ((mid . back . rounded . vowel) "ɔ")
          ((high . front . unrounded . vowel) "ɪ")
      ),
      ( "ɜ˞", -- , 025C 02DE, mid central tense rhotic,
        Monosegment $ (mid . front . tense . unrounded . V.rhotic . vowel) "ɛ"
      ),
      ( "ə˞", -- , 0259 02DE, mid central rhotic,
        Monosegment $ (mid . front . unrounded . V.rhotic . vowel) "ə"
      )
    ]
