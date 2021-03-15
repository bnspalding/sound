{-# LANGUAGE OverloadedStrings #-}

module Sound.Accents.GenAm.IPASpec
  ( spec,
  )
where

import Sound
import Sound.Accents.GenAm.IPA
import Test.Hspec

spec :: Spec
spec =
  describe "stringToIPASounds" $
    describe "test cases" $
      do
        it "case: oʊ -> o͡ʊ" $
          stringToIPASounds "boʊ" `shouldBe` Just (Sound <$> ["b", "o͡ʊ"])
        it "case: eɪ -> e͡ɪ" $
          stringToIPASounds "beɪ" `shouldBe` Just (Sound <$> ["b", "e͡ɪ"])
        it "case: aɪ -> a͡ɪ" $
          stringToIPASounds "baɪ" `shouldBe` Just (Sound <$> ["b", "a͡ɪ"])
        it "case: ɔɪ -> ɔ͡ɪ" $
          stringToIPASounds "bɔɪ" `shouldBe` Just (Sound <$> ["b", "ɔ͡ɪ"])
        it "case: aʊ -> a͡ʊ" $
          stringToIPASounds "baʊ" `shouldBe` Just (Sound <$> ["b", "a͡ʊ"])
        it "case: tʃ -> t͡ʃ" $
          stringToIPASounds "tʃiz" `shouldBe` Just (Sound <$> ["t͡ʃ", "i", "z"])
        it "case: dʒ -> d͡ʒ (dʒinz)" $
          stringToIPASounds "dʒinz" `shouldBe` Just (Sound <$> ["d͡ʒ", "i", "n", "z"])
        it "case: ɚ -> ə˞" $
          stringToIPASounds "pɚ" `shouldBe` Just (Sound <$> ["p", "ə˞"])
        it "case: ɝ -> ɜ˞" $
          stringToIPASounds "pɝ" `shouldBe` Just (Sound <$> ["p", "ɜ˞"])
        it "case: ɜɹ -> ɜ˞" $
          stringToIPASounds "pɜɹ" `shouldBe` Just (Sound <$> ["p", "ɜ˞"])
        it "case: ə(ɹ) -> ə˞" $
          stringToIPASounds "pə(ɹ)" `shouldBe` Just (Sound <$> ["p", "ə˞"])
        it "case: ɜ -> ɛ (tɜst -> tɛst)" $
          stringToIPASounds "tɜst" `shouldBe` Just (Sound <$> ["t", "ɛ", "s", "t"])
        it "case: ɛɹ -> ɛɹ (no change)" $
          stringToIPASounds "ɛɹ" `shouldBe` Just (Sound <$> ["ɛ", "ɹ"])
        it "case: ɛ˞ -> ɛɹ (not r-colored)" $
          stringToIPASounds "stɛ˞" `shouldBe` Just (Sound <$> ["s", "t", "ɛ", "ɹ"])
        it "case: ɛəɹ -> ɛɹ (ɛəɹ)" $
          stringToIPASounds "ɛəɹ" `shouldBe` Just (Sound <$> ["ɛ", "ɹ"])
        it "case: aɪə(ɹ) -> a͡ɪə˞" $
          stringToIPASounds "fa͡ɪə(ɹ)" `shouldBe` Just (Sound <$> ["f", "a͡ɪ", "ə˞"])
        it "case: ɪə(ɹ) -> ɪɹ" $
          stringToIPASounds "hɪə(ɹ)" `shouldBe` Just (Sound <$> ["h", "ɪ", "ɹ"])
        it "case: o͡ʊ -> o͡ʊ (preserve)" $
          stringToIPASounds "bo͡ʊ" `shouldBe` Just (Sound <$> ["b", "o͡ʊ"])
        it "case: e͡ɪ -> e͡ɪ (preserve)" $
          stringToIPASounds "be͡ɪ" `shouldBe` Just (Sound <$> ["b", "e͡ɪ"])
        it "case: a͡ɪ -> a͡ɪ (preserve)" $
          stringToIPASounds "ba͡ɪ" `shouldBe` Just (Sound <$> ["b", "a͡ɪ"])
        it "case: ɔ͡ɪ -> ɔ͡ɪ (preserve)" $
          stringToIPASounds "bɔ͡ɪ" `shouldBe` Just (Sound <$> ["b", "ɔ͡ɪ"])
        it "case: a͡ʊ -> a͡ʊ (preserve)" $
          stringToIPASounds "ba͡ʊ" `shouldBe` Just (Sound <$> ["b", "a͡ʊ"])
        it "case: y -> j" $
          stringToIPASounds "yɛs" `shouldBe` Just (Sound <$> ["j", "ɛ", "s"])
        it "case: q -> Nothing" $
          stringToIPASounds "qi" `shouldBe` Nothing
        it "case: a2 -> Nothing" $
          stringToIPASounds "ba2" `shouldBe` Nothing
