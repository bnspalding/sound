{-# LANGUAGE OverloadedStrings #-}

module Sound.GenAm.IPASpec
  ( spec,
  )
where

import Sound
import Sound.GenAm.IPA
import Test.Hspec

spec :: Spec
spec =
  describe "stringToIPASounds"
    $ describe "test cases"
    $ do
      it "case: oʊ -> o͡ʊ" $
        stringToIPASounds "boʊ" `shouldBe` Sound <$> ["b", "o͡ʊ"]
      it "case: eɪ -> e͡ɪ" $
        stringToIPASounds "beɪ" `shouldBe` Sound <$> ["b", "e͡ɪ"]
      it "case: aɪ -> a͡ɪ" $
        stringToIPASounds "baɪ" `shouldBe` Sound <$> ["b", "a͡ɪ"]
      it "case: ɔɪ -> ɔ͡ɪ" $
        stringToIPASounds "bɔɪ" `shouldBe` Sound <$> ["b", "ɔ͡ɪ"]
      it "case: aʊ -> a͡ʊ" $
        stringToIPASounds "baʊ" `shouldBe` Sound <$> ["b", "a͡ʊ"]
      it "case: ɚ -> ə˞" $ stringToIPASounds "pɚ" `shouldBe` Sound <$> ["p", "ə˞"]
      it "case: ɝ -> ɜ˞" $ stringToIPASounds "pɝ" `shouldBe` Sound <$> ["p", "ɜ˞"]
      it "case: ɜɹ -> ɜ˞" $
        stringToIPASounds "pɜɹ" `shouldBe` Sound <$> ["p", "ɜ˞"]
      it "case: ə(ɹ) -> ə˞" $
        stringToIPASounds "pə(ɹ)" `shouldBe` Sound <$> ["p", "ə˞"]
      it "case: tʃ -> t͡ʃ" $
        stringToIPASounds "tʃiz" `shouldBe` Sound <$> ["t͡ʃ", "i", "z"]
      it "case: dʒ -> d͡ʒ (dʒinz)" $
        stringToIPASounds "dʒinz" `shouldBe` Sound <$> ["d͡ʒ", "i", "n", "z"]
      it "case: ɜ -> ɛ (tɜst -> tɛst)" $
        stringToIPASounds "tɜst" `shouldBe` Sound <$> ["t", "ɛ", "s", "t"]
      it "case: ɛɹ -> ɛɹ (no change)" $
        stringToIPASounds "ɛɹ" `shouldBe` Sound <$> ["ɛ", "ɹ"]
      it "case: ɛ˞ -> ɛɹ (not r-colored)" $
        stringToIPASounds "stɛ˞" `shouldBe` Sound <$> ["s", "t", "ɛ", "ɹ"]
      it "case: ɛəɹ -> ɛɹ (ɛəɹ)" $
        stringToIPASounds "ɛəɹ" `shouldBe` Sound <$> ["ɛ", "ɹ"]
      it "case: o͡ʊ -> o͡ʊ (preserve)" $
        stringToIPASounds "bo͡ʊ" `shouldBe` Sound <$> ["b", "o͡ʊ"]
      it "case: e͡ɪ -> e͡ɪ (preserve)" $
        stringToIPASounds "be͡ɪ" `shouldBe` Sound <$> ["b", "e͡ɪ"]
      it "case: a͡ɪ -> a͡ɪ (preserve)" $
        stringToIPASounds "ba͡ɪ" `shouldBe` Sound <$> ["b", "a͡ɪ"]
      it "case: ɔ͡ɪ -> ɔ͡ɪ (preserve)" $
        stringToIPASounds "bɔ͡ɪ" `shouldBe` Sound <$> ["b", "ɔ͡ɪ"]
      it "case: a͡ʊ -> a͡ʊ (preserve)" $
        stringToIPASounds "ba͡ʊ" `shouldBe` Sound <$> ["b", "a͡ʊ"]
