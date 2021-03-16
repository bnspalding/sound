{-# LANGUAGE OverloadedStrings #-}

module Sound.PhonemeSpec (spec) where

import qualified Data.HashSet as HashSet
import Sound.Feature
import Sound.Phoneme
import Test.Hspec

spec :: Spec
spec =
  describe "Phoneme" $ do
    it "symbol 'p' -> p" $
      symbol testPhoneme `shouldBe` "p"
    it "features 'p' -> [featureSet]" $
      features testPhoneme
        `shouldBe` HashSet.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            MINUS_VOICE,
            LABIAL
          ]

testPhoneme :: Phoneme
testPhoneme =
  Phoneme
    { symbol = "p",
      features =
        HashSet.fromList
          [ MINUS_SYLLABIC,
            PLUS_CONSONANTAL,
            MINUS_SONORANT,
            MINUS_CONTINUANT,
            MINUS_VOICE,
            LABIAL
          ]
    }
