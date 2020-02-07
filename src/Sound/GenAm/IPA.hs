-- |
-- Module: Sound.GenAm.IPA
-- Description: convert IPA symbols to GenAm sounds
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Sound.IPA provides tools for converting strings of IPA symbols into sounds.
-- This is currently very much embedded in the "Sound.GenAm" mapping, which
-- means that certain symbols mean different things than they would in IPA
-- (mostly the symbols used for vowels).
module Sound.GenAm.IPA
  ( stringToIPASounds,
    textToIPASounds,
    normalize,
  )
where

import Data.List
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import Sound
import qualified Sound.GenAm as GenAm
import Sound.Stress
import Text.Replace

-- | stringToIPASounds converts a string into GenAm sounds. Non-IPA symbols
-- passed to stringToIPASounds will produce errors
stringToIPASounds :: String -> [Sound]
stringToIPASounds s =
  let toIPAIter sounds [] = sounds
      toIPAIter sounds xs = toIPAIter (sounds ++ [sound]) remaining
        where
          (sound, remaining) = nextSound xs ipaSymbols
   in toIPAIter [] (normalize s)

-- | textToIPASounds converts text into GenAm sounds. Non-IPA symbols passed to
-- textToIPASounds will produce errors
textToIPASounds :: T.Text -> [Sound]
textToIPASounds t = stringToIPASounds $ T.unpack t

-- | normalize performs a series of replacements on a string to simplify the IPA
-- symbols present in the string. Multi-character symbols (like tʃ or eɪ) are
-- joined with tie (becoming t͡ʃ and e͡ɪ respectively). A separate rhotic mark
-- (ə˞) is preferred over single symbols (ɚ) or an inverted r (əɹ) to mark
-- r-colored vowels. Whitespace is removed from the string.
normalize :: String -> String
normalize = replaceWithTrie repls . replaceWithTrie repls

repls :: Trie
repls =
  listToTrie
    [ Replace (s "oʊ") "o͡ʊ",
      Replace (s "eɪ") "e͡ɪ",
      Replace (s "aɪ") "a͡ɪ",
      Replace (s "ɔɪ") "ɔ͡ɪ",
      Replace (s "aʊ") "a͡ʊ",
      Replace (s "(ɹ)") "ɹ",
      Replace (s "ɚ") "ə˞",
      Replace (s "ɝ") "ɜ˞",
      Replace (s "ɜɹ") "ɜ˞",
      Replace (s "əɹ") "ə˞",
      Replace (s "tʃ") "t͡ʃ",
      Replace (s "dʒ") "d͡ʒ",
      Replace (s " ") "",
      Replace (s "\r") "",
      Replace (s "\t") "",
      Replace (s "\n") ""
    ]
  where
    s = string'fromString

stressSymbols :: [String]
stressSymbols = T.unpack <$> [stressSymbolIPA, secondaryStressSymbolIPA]

ipaSymbols :: [String]
ipaSymbols =
  ( sortOn (Down . length)
      . (++) stressSymbols
      . Set.toList
      . Set.map (\(Sound x) -> T.unpack x)
  )
    GenAm.sounds

nextSound :: String -> [String] -> (Sound, String)
nextSound [] _ = error "empty list given to nextSound in Sound.IPA"
nextSound (x : xs) [] = error $ "unknown symbol " ++ [x] ++ " in " ++ xs
nextSound xs (sym : syms) =
  if sym `isPrefixOf` xs
    then (Sound (T.pack sym), drop (length sym) xs)
    else nextSound xs syms
