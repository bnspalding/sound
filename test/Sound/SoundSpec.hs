{-# LANGUAGE OverloadedStrings #-}

module Sound.SoundSpec
  ( spec,
  )
where

import Sound.Sound
import Test.Hspec

spec :: Spec
spec =
  describe "Sound" $
    it "symbol (Sound \"h\") -> \"h\"" $
      symbol (Sound "h") `shouldBe` "h"
