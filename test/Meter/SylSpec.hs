{-# LANGUAGE OverloadedStrings #-}

module Meter.SylSpec
  ( spec,
  )
where

import Meter.Syl
import Sound
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

highStressSyl :: Syl
highStressSyl =
  Syl
    { onset = [Sound ""],
      nucleus = [Sound ""],
      coda = [Sound ""],
      stress = Just Stressed
    }

lowStressSyl :: Syl
lowStressSyl =
  Syl
    { onset = [Sound ""],
      nucleus = [Sound ""],
      coda = [Sound ""],
      stress = Just Unstressed
    }

reducedNothingSyl :: Syl
reducedNothingSyl =
  Syl
    { onset = [Sound ""],
      nucleus = [Sound "ə"],
      coda = [Sound ""],
      stress = Nothing
    }

unreducedNothingSyl :: Syl
unreducedNothingSyl =
  Syl
    { onset = [Sound ""],
      nucleus = [Sound "ɪ"],
      coda = [Sound ""],
      stress = Nothing
    }
