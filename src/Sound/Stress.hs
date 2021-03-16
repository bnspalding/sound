-- |
-- Module: Sound.Stress
-- Description: syllable stress
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Stress provides 4 levels of marked stress for a syllable. The four
-- levels of stress can also reduce to either high or low stress, when a binary
-- measure is more appropriate, using 'isLowStress' and 'isHighStress'.
--
-- Syllable level stress is intended to be lexical, i.e. only significant
-- between the syllables of a word. It is not recommended to use Stress to
-- describe levels of emphasis outside of this boundary. Monosyllabic words
-- should not be marked with any level of stress (because there is nothing to
-- compare it to).
--
-- There doesn't appear to be solid agreement about how many levels of
-- distinguishable stress are useful (particularly in a GenAm context), so the
-- choice to use 4 (with means for reducing to 2) is motivated by leaving the
-- greatest number of options open. The CMU Pronouncing Dictionary uses 3
-- levels, which map 0-Unstressed 1-Stressed 2-SecondaryStress.
module Sound.Stress where

-- | Stress is represented with four levels of emphasis. Use 'isLowStress' and
-- 'isHighStress' to reduce to binary stress.
data Stress
  = ReducedStress
  | Unstressed
  | SecondaryStress
  | Stressed
  deriving (Ord, Eq, Show)

-- | Unstressed and ReducedStress are both Low Stress. NullStress is neither Low
-- nor High Stress.
isLowStress :: Stress -> Bool
isLowStress s = s <= Unstressed

-- | Stressed and SecondaryStress are both High Stress. NullStress is neither
-- Low nor High Stress.
isHighStress :: Stress -> Bool
isHighStress s = s >= SecondaryStress

-- | symbol provides the lexical stress marker for a stress level (if one
-- exists)
--
-- In the future, this should get moved out to the mapping information in
-- order to support mappings other than IPA.
symbol :: Stress -> Maybe Char
symbol Stressed = Just stressSymbolIPA
symbol SecondaryStress = Just secondaryStressSymbolIPA
symbol _ = Nothing

-- | stressSymbolIPA is the lexical stress marker used by IPA to mark stress:
-- ˈ (Unicode U+02C8 "Modifier Letter Vertical Line")
stressSymbolIPA :: Char
stressSymbolIPA = 'ˈ'

-- | secondaryStressSymbolIPA is the lexical stress marker used by IPA to mark
-- secondary stress: ˌ (Unicode U+02CC "Modifier Letter Low Vertical Line")
secondaryStressSymbolIPA :: Char
secondaryStressSymbolIPA = 'ˌ'
