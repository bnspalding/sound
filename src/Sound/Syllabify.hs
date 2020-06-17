{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Sound.Syllabify
-- Description: automatic syllabification
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.Syllabify automatically breaks a sequence of sounds into syllables,
-- according to the differences in sonority between sounds. The rules used to
-- break syllables are language specific (in this case English), and they work
-- __most__ of the time.
--
-- See <https://en.wikipedia.org/wiki/Sonority_hierarchy> for more information.
module Sound.Syllabify
  ( syllabify,
  )
where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Sound
import Sound.Accents.GenAm
import Sound.Feature
import Sound.Stress

-- | syllabify breaks a sequence of sounds into syllables. Syllabify should be
-- given a single word's worth of sounds, as syllable boundaries don't hold up
-- across multiple words.
--
-- syllabify also accepts IPA stress and syllable break symbols (ˈ, ˌ, .) as
-- Sounds. It uses these to define syllable breaks and add stress information to
-- the syllables. Following IPA convention, the stress mark should be placed at
-- the beginning of the syllable (not on the vowel), as it is used to mark
-- syllable breaks.
syllabify :: [Sound] -> Sound.Word
syllabify [] = []
syllabify ss = _syllabify [] [] Nothing ss -- iterative recursive syllabify

-- _syllabify iterates over the given list of sounds and uses the direction of
-- sonority between sounds (less-to-more, more-to-less) to find syllable breaks.
-- It also looks for break symbols (such as stress symbols or pre-inserted
-- syllable breaks), which override the sonority direction logic.
--
-- args     : result -> current syl -> sonority direction -> unprocessed -> word
_syllabify :: [[Sound]] -> [Sound] -> Maybe SonorityDir -> [Sound] -> Sound.Word
-- Empty Case: do nothing when supplied an empty initial sound list
_syllabify _ _ _ [] = []
-- End Case: package up the result when the end of the list is reached
_syllabify result currentSyl prevDir [current] =
  let final =
        if containsVowel (currentSyl ++ [current])
          then-- special case: FLAT vowel at end creates new syllable

            if prevDir == Just FLAT && maybe False isVowel (features current)
              then result ++ [currentSyl] ++ [[current]]
              else result ++ [currentSyl ++ [current]]
          else init result ++ [last result ++ currentSyl ++ [current]]
   in makeSyl <$> filter (not . null) final
-- Recursive Case: break the sound list into sublists at breakpoints
_syllabify result currentSyl prevDir (current : next : ss) =
  let currentDir = direction current next -- sonority direction
      (_result, _currentSyl) =
        -- the accumulated syls and the working syl
        if isBreakSym current || shouldBreak prevDir currentDir
          then (result ++ [currentSyl], [current]) -- shift to a new syl
          else (result, currentSyl ++ [current]) -- add to current syl
   in _syllabify _result _currentSyl currentDir (next : ss)

type Sonority = Int

sonority :: Sound -> Sonority
sonority s
  | s == Sound "s" = 0 -- "s" is a special case
  | not (isVoiced fs) && isStop fs = 1 -- voiceless plosives
  | isVoiced fs && isStop fs = 2 -- voiced plosives
  | not (isVoiced fs) && isFricative fs = 3 -- voiceles fricatives
  | isVoiced fs && isFricative fs = 4 -- voiced fricatives
  | isAffricate fs = 4 -- affricates
  | isNasal fs = 5 -- nasals
  | isLateral fs = 6 -- laterals
  | isApproximant fs || isGlide fs = 7 -- approximants and glides
  | isVowel fs = 10 -- vowels (inner-vowel differentiation not helpful here)
    --  | isHighVowel fs = 8
    --  | isMidVowel fs = 9
    --  | isLowVowel fs = 10
  | otherwise = 0
  where
    fs = fromMaybe (featureSet []) (features s)

data SonorityDir
  = UP
  | DOWN
  | FLAT
  deriving (Ord, Eq)

-- direction looks ahead to the next symbol and returns the sonority direction
-- from the current sound to the next.
--
-- Because stress and syl-break symbols are represented as sounds, there is a
-- case where direction does not exist.
direction :: Sound -> Sound -> Maybe SonorityDir
direction s1 s2
  | isBreakSym s1 = Nothing
  | sonority s1 == sonority s2 = Just FLAT
  | sonority s1 < sonority s2 = Just UP
  | otherwise = Just DOWN

-- syllables should be broken up at FLATs or DOWN-to-UP inflection points
shouldBreak :: Maybe SonorityDir -> Maybe SonorityDir -> Bool
shouldBreak (Just FLAT) _ = True
shouldBreak (Just DOWN) (Just UP) = True
shouldBreak _ _ = False

makeSyl :: [Sound] -> Syl
makeSyl [] = error "no sounds passed to makeSyl"
makeSyl soundList =
  Syl
    { onset = before,
      nucleus = [mostSonorous],
      coda = after,
      stress = Just (stressFromMaybe stressSymMaybe mostSonorous)
    }
  where
    (stressSymMaybe, ss) = extractStressSym soundList
    (mostSonorous, mostSonorousI) =
      maximumBy (comparing (sonority . fst)) (zip ss [0 ..])
    (before, after) =
      case splitAt mostSonorousI ss of
        (_before, []) -> (_before, [])
        (_before, _ : _after) -> (_before, _after)

stressSymsIPA :: [T.Text]
stressSymsIPA = T.singleton <$> [stressSymbolIPA, secondaryStressSymbolIPA]

breakSymsIPA :: [T.Text]
breakSymsIPA = "." : stressSymsIPA

isBreakSym :: Sound -> Bool
isBreakSym (Sound x) = x `elem` breakSymsIPA

-- also extracting sylBreak (this should be moved to syllabization above)
extractStressSym :: [Sound] -> (Maybe Sound, [Sound])
extractStressSym s = (stressMaybe, stressRemoved)
  where
    stressMaybe =
      case stressExtracted of
        [] -> Nothing
        [x] -> Just x
        _ -> error "multiple stress symbols found in one syllable"
    stressExtracted = filter (\(Sound x) -> x `elem` ("." : stressSymsIPA)) s
    stressRemoved = filter (`notElem` stressExtracted) s

stressFromMaybe :: Maybe Sound -> Sound -> Stress
stressFromMaybe s (Sound nuc) =
  if nuc == "ə" || nuc == "ə˞"
    then ReducedStress
    else case s of
      Nothing -> Unstressed
      Just (Sound ".") -> Unstressed --this is from extracting above. should be fixed
      Just (Sound "ˈ") -> Stressed
      Just (Sound "ˌ") -> SecondaryStress
      _ -> error $ "unknown stress symbol: " ++ show s

containsVowel :: [Sound] -> Bool
containsVowel = any (isVowel . featuresOrEmpty . features)
