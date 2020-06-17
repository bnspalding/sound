{-# LANGUAGE OverloadedStrings #-}

module Sound.SyllabifySpec
  ( spec,
  )
where

import qualified Data.Text as T
import Sound.Sound
import Sound.Stress
import Sound.Syl
import Sound.Syllabify
import Test.Hspec

spec :: Spec
spec =
  describe "syllabify" $ do
    describe "test cases" $ do
      it "case: ɹɪpla͡ɪ -> ɹɪ.pla͡ɪ" $
        syllabify (Sound <$> ["ɹ", "ɪ", "p", "l", "a͡ɪ"])
          `shouldBe` [s ["ɹ"] ["ɪ"] [], s ["p", "l"] ["a͡ɪ"] []]
      it "case: ɹɛpta͡ɪl -> ɹɛp.ta͡ɪl" $
        syllabify (Sound <$> ["ɹ", "ɛ", "p", "t", "a͡ɪ", "l"])
          `shouldBe` [s ["ɹ"] ["ɛ"] ["p"], s ["t"] ["a͡ɪ"] ["l"]]
      it "case: ɹɛplɪke͡ɪt -> ɹɛ.plɪ.ke͡ɪt" $
        syllabify (Sound <$> ["ɹ", "ɛ", "p", "l", "ɪ", "k", "e͡ɪ", "t"])
          `shouldBe` [s ["ɹ"] ["ɛ"] [], s ["p", "l"] ["ɪ"] [], s ["k"] ["e͡ɪ"] ["t"]]
      it "case: wɪlmɪŋtʌn -> wɪl.mɪŋ.tʌn" $
        syllabify (Sound <$> ["w", "ɪ", "l", "m", "ɪ", "ŋ", "t", "ʌ", "n"])
          `shouldBe` [s ["w"] ["ɪ"] ["l"], s ["m"] ["ɪ"] ["ŋ"], s ["t"] ["ʌ"] ["n"]]
      it "case: kʌnstɹe͡ɪnt -> kʌn.stɹe͡ɪnt" $
        syllabify (Sound <$> ["k", "ʌ", "n", "s", "t", "ɹ", "e͡ɪ", "n", "t"])
          `shouldBe` [s ["k"] ["ʌ"] ["n"], s ["s", "t", "ɹ"] ["e͡ɪ"] ["n", "t"]]
      it "case: wɛstə˞n -> wɛ.stə˞n" $
        syllabify (Sound <$> ["w", "ɛ", "s", "t", "ə˞", "n"])
          `shouldBe` [s ["w"] ["ɛ"] [], s ["s", "t"] ["ə˞"] ["n"]]
      it "case: pænts -> pænts" $
        syllabify (Sound <$> ["p", "æ", "n", "t", "s"])
          `shouldBe` [s ["p"] ["æ"] ["n", "t", "s"]]
      it "case: skwɜ˞əl -> skwɜ˞.əl" $
        syllabify (Sound <$> ["s", "k", "w", "ɜ˞", "ə", "l"])
          `shouldBe` [s ["s", "k", "w"] ["ɜ˞"] [], s [] ["ə"] ["l"]]
      it "case: læf -> læf" $
        syllabify (Sound <$> ["l", "æ", "f"]) `shouldBe` [s ["l"] ["æ"] ["f"]]
      it "case: pɹɛʃə˞ -> pɹɛ.ʃə˞" $
        syllabify (Sound <$> ["p", "ɹ", "ɛ", "ʃ", "ə˞"])
          `shouldBe` [s ["p", "ɹ"] ["ɛ"] [], s ["ʃ"] ["ə˞"] []]
      it "case: ɹɛt͡ʃɪd -> ɹɛ.t͡ʃɪd" $
        syllabify (Sound <$> ["ɹ", "ɛ", "t͡ʃ", "ɪ", "d"])
          `shouldBe` [s ["ɹ"] ["ɛ"] [], s ["t͡ʃ"] ["ɪ"] ["d"]]
      it "case: pɑɑ -> pɑ.ɑ" $
        syllabify (Sound <$> ["p", "ɑ", "ɑ"])
          `shouldBe` [s ["p"] ["ɑ"] [], s [] ["ɑ"] []]
      it "case: pɑɑl -> pɑ.ɑl" $
        syllabify (Sound <$> ["p", "ɑ", "ɑ", "l"])
          `shouldBe` [s ["p"] ["ɑ"] [], s [] ["ɑ"] ["l"]]
      it "case: t͡ʃa͡ʊə˞i -> t͡ʃa͡ʊ.ə˞.i" $
        syllabify (Sound <$> ["t͡ʃ", "a͡ʊ", "ə˞", "i"])
          `shouldBe` [s ["t͡ʃ"] ["a͡ʊ"] [], s [] ["ə˞"] [], s [] ["i"] []]
      it "case: ˈkæmpˌfa͡ɪə˞ -> ˈkæmpˌfa͡ɪ.ə˞" $
        syllabify (Sound <$> ["ˈ", "k", "æ", "m", "p", "ˌ", "f", "a͡ɪ", "ə˞"])
          `shouldBe` [ s' ["k"] ["æ"] ["m", "p"] Stressed,
                       s' ["f"] ["a͡ɪ"] [] SecondaryStress,
                       s [] ["ə˞"] []
                     ]
      it "case: kɹʌst -> kɹʌst" $
        syllabify (Sound <$> ["k", "ɹ", "ʌ", "s", "t"])
          `shouldBe` [s ["k", "ɹ"] ["ʌ"] ["s", "t"]]
      it "case: ɜ˞stwa͡ɪl -> ɜ˞.stwa͡ɪl" $
        syllabify (Sound <$> ["ɜ˞", "s", "t", "w", "a͡ɪ", "l"])
          `shouldBe` [s [] ["ɜ˞"] [], s ["s", "t", "w"] ["a͡ɪ"] ["l"]]
      it "case: dæməsk -> dæ.məsk" $
        syllabify (Sound <$> ["d", "æ", "m", "ə", "s", "k"])
          `shouldBe` [s ["d"] ["æ"] [], s ["m"] ["ə"] ["s", "k"]]
      it "case: kɹɪpt -> kɹɪpt" $
        syllabify (Sound <$> ["k", "ɹ", "ɪ", "p", "t"])
          `shouldBe` [s ["k", "ɹ"] ["ɪ"] ["p", "t"]]
      it "case: plɑ.t.kɑ -> plɑ.t.kɑ (arbitrary)" $
        syllabify (Sound <$> ["p", "l", "ɑ", ".", "t", ".", "k", "ɑ"])
          `shouldBe` [s ["p", "l"] ["ɑ"] [], s [] ["t"] [], s ["k"] ["ɑ"] []]
      it "case: ˈstɪ.əˌkʊt -> stɪ.nə.kʊt (arbitrary)" $
        syllabify (Sound <$> ["ˈ", "s", "t", "ɪ", ".", "ə", "ˌ", "k", "ʊ", "t"])
          `shouldBe` [ s' ["s", "t"] ["ɪ"] [] Stressed,
                       s [] ["ə"] [],
                       s' ["k"] ["ʊ"] ["t"] SecondaryStress
                     ]
    describe "empty list"
      $ it "returns an empty syl set"
      $ syllabify [] `shouldBe` []
    describe "unknown input"
      $ it "silently accepts the unknown symbols as sonority 0"
      $ syllabify (Sound <$> ["p", "2", "ɑ"])
        `shouldBe` [s [] ["p"] [], s ["2"] ["ɑ"] []]

s :: [T.Text] -> [T.Text] -> [T.Text] -> Syl
s _onset _nucleus _coda =
  Syl
    { onset = Sound <$> _onset,
      nucleus = Sound <$> _nucleus,
      coda = Sound <$> _coda,
      stress = Just Unstressed
    }

s' :: [T.Text] -> [T.Text] -> [T.Text] -> Stress -> Syl
s' _onset _nucleus _coda _stress =
  Syl
    { onset = Sound <$> _onset,
      nucleus = Sound <$> _nucleus,
      coda = Sound <$> _coda,
      stress = Just _stress
    }
