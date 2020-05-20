{-# LANGUAGE OverloadedStrings #-}

module Sound.WordSpec
  ( spec,
  )
where

import Sound.Sound
import Sound.Stress
import qualified Sound.Syl as Syl
import qualified Sound.Word as W
import Test.Hspec

spec :: Spec
spec = do
  describe "Word" $ do
    it "length 'pʌmp.kɪn' -> 2" $ length demoWord `shouldBe` 2
    it "stress 'pʌmp.kɪn' -> [Stressed, Unstressed] " $
      W.stress demoWord `shouldBe` [Stressed, Unstressed]
    it "sounds 'pʌmp.kɪn' -> [p, ʌ, m, p, k, ɪ, n] " $
      W.sounds demoWord
        `shouldBe` (Sound <$> ["p", "ʌ", "m", "p", "k", "ɪ", "n"])
  describe "symbols"
    $ it "converts a word to textual representation, grouped by syl"
    $ W.symbols demoWord `shouldBe` "ˈpʌmp.kɪn"

demoWord :: W.Word
demoWord =
  [ Syl.Syl
      { Syl.onset = Sound <$> ["p"],
        Syl.nucleus = Sound <$> ["ʌ"],
        Syl.coda = Sound <$> ["m", "p"],
        Syl.stress = Stressed
      },
    Syl.Syl
      { Syl.onset = Sound <$> ["k"],
        Syl.nucleus = Sound <$> ["ɪ"],
        Syl.coda = Sound <$> ["n"],
        Syl.stress = Unstressed
      }
  ]
