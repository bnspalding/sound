{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Meter.Syllable
-- Description: evaluating meter on syllables
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Meter.Syllable provides tools for fitting syllables to meter. The words
-- 'macron' and 'breve' come from the symbols used to mark accented/heavy/
-- stressed and unaccented/light/unstressed syllables in poetic meter. While
-- they take a little bit of getting used to, I think they are clearer than
-- overloading the word \"stress\" with yet another meaning.
--
-- These functions rely on the convention that monosyllabic words have their
-- stress marked as Nothing. Be careful if you are trying to work with meter in
-- a context where you mean for stress values of Nothing to mean something else.
module Meter.Syllable
  ( macron,
    breve,
    breve',
  )
where

import Sound.Phoneme (symbol)
import Sound.Stress (isHighStress, isLowStress)
import Sound.Syllable

-- | macron matches stressed (accented, heavy) syllables
--
-- High stress syllables in polysyllabic words and non-reduced syllables
-- in monosyllabic words are considered a match.
macron :: Syllable -> Bool
macron syl = maybe (not (isReducedVowel syl)) isHighStress (stress syl)

-- | breve matches unstressed (unaccented, light) syllables
--
-- Low stress syllables in polysyllabic words and ONLY reduced syllables in
-- monosyllabic words are considered a match.
breve :: Syllable -> Bool
breve syl = maybe (isReducedVowel syl) isLowStress (stress syl)

-- | breve' matches unstressed (unaccented, light) syllables more permissively
--
-- Low stress syllables in polysyllabic words and all monosyllabic words are
-- considered a match
breve' :: Syllable -> Bool
breve' syl = maybe True isLowStress (stress syl)

isReducedVowel :: Syllable -> Bool
isReducedVowel syl = nuc == "ə" || nuc == "ə˞"
  where
    nuc = symbol $ nucleus syl
