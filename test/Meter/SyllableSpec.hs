{-# LANGUAGE OverloadedStrings #-}

module Meter.SyllableSpec
  ( spec,
  )
where

import Meter.Syllable
import Sound
import Sound.Utilities
import Test.Hspec

spec :: Spec
spec = do
  describe "macron" $ do
    it "matches high stress syllables" $
      macron highStressSyl `shouldBe` True
    it "matches unreduced vowel Nothing" $
      macron unreducedNothingSyl `shouldBe` True
    it "rejects low stress syllables" $
      macron lowStressSyl `shouldBe` False
    it "rejects reduced vowel Nothing" $
      macron reducedNothingSyl `shouldBe` False
  describe "breve" $ do
    it "matches low stress syllables" $
      breve lowStressSyl `shouldBe` True
    it "matches reduced vowel Nothing" $
      breve reducedNothingSyl `shouldBe` True
    it "rejects high stress syllables" $
      breve highStressSyl `shouldBe` False
    it "rejects unreduced vowel Nothing" $
      breve unreducedNothingSyl `shouldBe` False
  describe "breve' (permissive breve)" $ do
    it "matches low stress syllables" $
      breve' lowStressSyl `shouldBe` True
    it "matches reduced vowel Nothing" $
      breve' reducedNothingSyl `shouldBe` True
    it "matches unreduced vowel Nothing" $
      breve' unreducedNothingSyl `shouldBe` True
    it "rejects high stress syllables" $
      breve' highStressSyl `shouldBe` False

highStressSyl :: Syllable
highStressSyl =
  Syllable
    { onset = [mkPhoneme ""],
      nucleus = mkPhoneme "",
      coda = [mkPhoneme ""],
      stress = Just Stressed
    }

lowStressSyl :: Syllable
lowStressSyl =
  Syllable
    { onset = [mkPhoneme ""],
      nucleus = mkPhoneme "",
      coda = [mkPhoneme ""],
      stress = Just Unstressed
    }

reducedNothingSyl :: Syllable
reducedNothingSyl =
  Syllable
    { onset = [mkPhoneme ""],
      nucleus = mkPhoneme "ə",
      coda = [mkPhoneme ""],
      stress = Nothing
    }

unreducedNothingSyl :: Syllable
unreducedNothingSyl =
  Syllable
    { onset = [mkPhoneme ""],
      nucleus = mkPhoneme "ɪ",
      coda = [mkPhoneme ""],
      stress = Nothing
    }
