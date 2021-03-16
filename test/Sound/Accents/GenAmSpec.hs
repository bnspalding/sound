{-# LANGUAGE OverloadedStrings #-}

module Sound.Accents.GenAmSpec
  ( spec,
  )
where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Sound.Accents.GenAm as GenAm
import Test.Hspec

spec :: Spec
spec =
  describe "data correctness" $ do
    it "consonants are: m n ŋ p b t d k ɡ t͡ʃ d͡ʒ f v θ ð s z ʃ ʒ h l ɹ j ʍ w" $
      GenAm.symbols `shouldSatisfy` HashSet.isSubsetOf consonants
    it "vowels are: i ɪ ɛ æ ə ʌ ɑ u ʊ ɔ e͡ɪ a͡ɪ a͡ʊ o͡ʊ ɔ͡ɪ ɜ˞ ə˞" $
      GenAm.symbols `shouldSatisfy` HashSet.isSubsetOf vowels
    it "does not contain any other symbols than those given above" $
      GenAm.symbols `shouldSatisfy` (== allSounds)
    it "does not contain common mistypes: e a o r g ɑ͡ɪ ɛ˞ y" $
      HashSet.intersection GenAm.symbols mistypes `shouldBe` HashSet.empty

allSounds :: HashSet.HashSet T.Text
allSounds = vowels <> consonants

consonants :: HashSet.HashSet T.Text
consonants =
  HashSet.fromList
    [ "m",
      "n",
      "ŋ",
      "p",
      "b",
      "t",
      "d",
      "k",
      "ɡ",
      "t͡ʃ",
      "d͡ʒ",
      "f",
      "v",
      "θ",
      "ð",
      "s",
      "z",
      "ʃ",
      "ʒ",
      "h",
      "l",
      "ɹ",
      "j",
      "ʍ",
      "w"
    ]

vowels :: HashSet.HashSet T.Text
vowels =
  HashSet.fromList
    [ "i",
      "ɪ",
      "ɛ",
      "æ",
      "ə",
      "ʌ",
      "ɑ",
      "u",
      "ʊ",
      "ɔ",
      "e͡ɪ",
      "a͡ɪ",
      "a͡ʊ",
      "o͡ʊ",
      "ɔ͡ɪ",
      "ɜ˞",
      "ə˞"
    ]

mistypes :: HashSet.HashSet T.Text
mistypes = HashSet.fromList ["e", "a", "o", "r", "g", "ɑ͡ɪ", "ɛ˞", "y"]
