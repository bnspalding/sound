{-# LANGUAGE OverloadedStrings #-}

module Rhyme.StrictSpec
  ( spec,
  )
where

import qualified Rhyme.Strict as Strict
import Sound.Syllable hiding (rhyme)
import Sound.Utilities
import Test.Hspec

spec :: Spec
spec = do
  describe "rhyme" $ do
    it "checks equality between the rhymes of two syls" $
      Strict.rhyme demoSylTik demoSylTok `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.rhyme demoSylTik demoSylTik `shouldBe` True
  describe "assonance" $ do
    it "checks equality betwee the nuclei of two syls" $
      Strict.assonance demoSylTik demoSylTok `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.assonance demoSylTik demoSylShift `shouldBe` True
  describe "alliteration" $ do
    it "checks equality between the onsets of two syls" $
      Strict.alliteration demoSylTik demoSylShift `shouldBe` False
    it "is true only when the sounds are the same" $
      Strict.alliteration demoSylTik demoSylTok `shouldBe` True

demoSylTik :: Syllable
demoSylTik =
  Syllable
    { onset = mkPhoneme <$> ["t"],
      nucleus = mkPhoneme "ɪ",
      coda = mkPhoneme <$> ["k"],
      stress = Nothing
    }

demoSylTok :: Syllable
demoSylTok =
  Syllable
    { onset = mkPhoneme <$> ["t"],
      nucleus = mkPhoneme "ɔ",
      coda = mkPhoneme <$> ["k"],
      stress = Nothing
    }

demoSylShift :: Syllable
demoSylShift =
  Syllable
    { onset = mkPhoneme <$> ["ʃ"],
      nucleus = mkPhoneme "ɪ",
      coda = mkPhoneme <$> ["f", "t"],
      stress = Nothing
    }
