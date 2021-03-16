{-# LANGUAGE OverloadedStrings #-}

module Sound.SyllableSpec
  ( spec,
  )
where

import Sound.Stress
import Sound.Syllable
import Sound.Utilities
import Test.Hspec

spec :: Spec
spec =
  describe "Syl" $ do
    it "onset 'p l ɔ p s' -> [p, l]" $
      onset demoSyl `shouldBe` (mkPhoneme <$> ["p", "l"])
    it "nucleus 'p l ɔ p s' -> [ɔ]" $
      nucleus demoSyl `shouldBe` (mkPhoneme "ɔ")
    it "coda 'p l ɔ p s' -> [p, s]" $
      coda demoSyl `shouldBe` (mkPhoneme <$> ["p", "s"])
    it "rhyme 'p l ɔ p s' -> [ɔ, p, s]" $
      rhyme demoSyl `shouldBe` (mkPhoneme <$> ["ɔ", "p", "s"])
    it "sounds 'p l ɔ p s' -> [p, l, ɔ, p, s]" $
      phonemes demoSyl `shouldBe` (mkPhoneme <$> ["p", "l", "ɔ", "p", "s"])
    it "stress 'p l ɔ p s' -> Just Stressed" $
      stress demoSyl `shouldBe` Just Stressed
    it "symbols 'p l ɔ p s -> \"ˈplɔps\"" $
      symbols demoSyl `shouldBe` "ˈplɔps"

demoSyl :: Syllable
demoSyl =
  Syllable
    { onset = mkPhoneme <$> ["p", "l"],
      nucleus = mkPhoneme "ɔ",
      coda = mkPhoneme <$> ["p", "s"],
      stress = Just Stressed
    }
