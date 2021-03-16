{-# LANGUAGE OverloadedStrings #-}

module Sound.WordSpec
  ( spec,
  )
where

import Sound.Stress
import Sound.Syllable hiding (phonemes, symbols)
import Sound.Utilities
import Sound.Word
import Test.Hspec
import Prelude hiding (Word)

spec :: Spec
spec =
  describe "Word" $ do
    it "length 'pʌmp.kɪn' -> 2" $ length demoWord `shouldBe` 2
    it "stress 'pʌmp.kɪn' -> [Stressed, Unstressed] " $
      stresses demoWord `shouldBe` [Just Stressed, Just Unstressed]
    it "sounds 'pʌmp.kɪn' -> [p, ʌ, m, p, k, ɪ, n] " $
      phonemes demoWord
        `shouldBe` (mkPhoneme <$> ["p", "ʌ", "m", "p", "k", "ɪ", "n"])
    it "symbols 'pʌmp.kɪn' -> \"ˈpʌmp.kɪn\"" $
      symbols demoWord `shouldBe` "ˈpʌmp.kɪn"

demoWord :: Word
demoWord =
  [ Syllable
      { onset = mkPhoneme <$> ["p"],
        nucleus = mkPhoneme "ʌ",
        coda = mkPhoneme <$> ["m", "p"],
        stress = Just Stressed
      },
    Syllable
      { onset = mkPhoneme <$> ["k"],
        nucleus = mkPhoneme "ɪ",
        coda = mkPhoneme <$> ["n"],
        stress = Just Unstressed
      }
  ]
