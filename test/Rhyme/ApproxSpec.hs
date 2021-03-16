{-# LANGUAGE OverloadedStrings #-}

module Rhyme.ApproxSpec
  ( spec,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Rhyme.Approx as Approx
import Sound.Accents.GenAm (phoneme)
import Sound.Phoneme
import Sound.Syllable hiding (rhyme)
import Test.Hspec

spec :: Spec
spec = do
  describe "rhyme" $ do
    it "measures the feature similarity between the rhymes of two syls" $
      Approx.rhyme demoSylTruck demoSylTorque
        `shouldSatisfy` (> Approx.rhyme demoSylTruck demoSylShaft)
    it "is normalized between the numbers 0 and 1" $
      Approx.rhyme demoSylTruck demoSylTorque
        `shouldSatisfy` (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.rhyme demoSylTruck demoSylTruck `shouldSatisfy` (== 1)
  describe "assonance" $ do
    it "measures the feature similarity between the nuclei of two syls" $
      Approx.assonance demoSylTruck demoSylTorque
        `shouldSatisfy` (> Approx.assonance demoSylTruck demoSylShaft)
    it "is normalized between the numbers 0 and 1" $
      Approx.assonance demoSylTruck demoSylTorque
        `shouldSatisfy` (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.assonance demoSylTruck demoSylTruck `shouldSatisfy` (== 1)
  describe "alliteration" $ do
    it "measures the feature similarity between the onsets of two syls" $
      Approx.alliteration demoSylTruck demoSylTorque
        `shouldSatisfy` (> Approx.alliteration demoSylTruck demoSylShaft)
    it "is normalized between the numbers 0 and 1" $
      Approx.alliteration demoSylTruck demoSylTorque
        `shouldSatisfy` (\x -> x >= 0 && x <= 1)
    it "should equal 1 when a syl is compared to itself" $
      Approx.alliteration demoSylTruck demoSylTruck `shouldSatisfy` (== 1)

demoSylTruck :: Syllable
demoSylTruck =
  Syllable
    { onset = mkGenAm <$> ["t", "ɹ"],
      nucleus = mkGenAm "ʌ",
      coda = mkGenAm <$> ["k"],
      stress = Nothing
    }

demoSylTorque :: Syllable
demoSylTorque =
  Syllable
    { onset = mkGenAm <$> ["t"],
      nucleus = mkGenAm "ɔ",
      coda = mkGenAm <$> ["ɹ", "k"],
      stress = Nothing
    }

demoSylShaft :: Syllable
demoSylShaft =
  Syllable
    { onset = mkGenAm <$> ["ʃ"],
      nucleus = mkGenAm "æ",
      coda = mkGenAm <$> ["f", "t"],
      stress = Nothing
    }

mkGenAm :: T.Text -> Phoneme
mkGenAm = fromJust . phoneme
