{-# LANGUAGE OverloadedStrings #-}

module Sound.SoundSpec
  ( spec,
  )
where

import Sound.Sound
import Test.Hspec

spec :: Spec
spec =
  describe "Sound"
    $ describe "symbol"
    $ it "should return the symbolic representation of the sound"
    $ symbol (Sound "h") `shouldBe` "h"
