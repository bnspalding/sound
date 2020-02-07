{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Sound.Stress
-- Description: syllable stress
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Stress provides 4 levels of marked stress for a syllable, as well as a
-- null stress option for when stress is not known. The four levels of stress
-- can also reduce to either high or low stress, when a binary measure is
-- more appropriate, using 'isLowStress' and 'isHighStress'
--
-- There doesn't appear to be solid agreement about how many levels of
-- distinguishable stress are useful (particularly in a GenAm context), so the
-- choice to use 4 (with means for reducing to 2) is motivated by leaving the
-- greatest number of options open. The CMU Pronouncing Dictionary uses 3
-- levels, which map 0-Unstressed 1-Stressed 2-SecondaryStress.
module Sound.Stress where

import qualified Data.Text as T

-- | Stress comes in 4 levels, as well as NullStress, which can be used when
-- stress information is not available.
data Stress
  = NullStress
  | ReducedStress
  | Unstressed
  | SecondaryStress
  | Stressed
  deriving (Ord, Eq, Show)

-- | Unstressed and ReducedStress are both Low Stress. NullStress is neither Low
-- nor High Stress.
isLowStress :: Stress -> Bool
isLowStress NullStress = False
isLowStress s = s <= Unstressed

-- | Stressed and SecondaryStress are both High Stress. NullStress is neither
-- Low nor High Stress.
isHighStress :: Stress -> Bool
isHighStress s = s >= SecondaryStress

-- | maybeStress distinguishes between NullStress and actual Stress levels.
maybeStress :: Stress -> Maybe Stress
maybeStress NullStress = Nothing
maybeStress s = Just s

-- | stressSymbolIPA is the lexical stress marker used by IPA to mark stress
stressSymbolIPA :: T.Text
stressSymbolIPA = "ˈ" -- 02C8

-- | secondaryStressSymbolIPA is the lexical stress marker used by IPA to mark
-- secondary stress.
secondaryStressSymbolIPA :: T.Text
secondaryStressSymbolIPA = "ˌ" --02CC
