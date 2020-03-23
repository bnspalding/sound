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
stringToIPASounds str =
  let toIPAIter sounds [] = sounds
      toIPAIter sounds xs = toIPAIter (sounds ++ [sound]) remaining
        where
          (sound, remaining) = nextSound xs ipaSymbols
   in toIPAIter [] (normalize str)

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
normalize =
  let r = replaceWithTrie
   in r replNonChar . r replThrd . r replSnd . r replFst . r replZero

-- NOTE: the string is searched for replacements left to right, and longer
-- replacements are preferred over shorter ones. In some cases (r's, ɜ's) a certain
-- number of successive replacements (see "normalize") is required to get things
-- right. This is not the greatest, and should be perhaps reworked later.
-- NOTE: really, instead of replacing on symbols, we should be replacing on
-- sounds in order to reduce to GenAm. However, that requires that we recognize
-- all of the IPA sounds and then reduce, which is a good bit more work.
-- TODO: Something is breaking with rhotics after doing the reverse epsilon
-- replacement. Run tests to see issues.
replZero :: Trie
replZero =
  listToTrie
    [ Replace (s "r") "ɹ"
    ]

replFst :: Trie
replFst =
  listToTrie
    [ Replace (s "ɒ") "ɑ",
      Replace (s "ʀ") "ɹ",
      Replace (s "ɐ") "ə", -- this is a quick judgement
      Replace (s "e") "ə", -- partially reversed later [a]
      Replace (s "ɜ") "ɛ", -- partially reversed later [b]
      Replace (s "a") "æ", -- partially reversed later [c]
      Replace (s "n̩") "ən",
      Replace (s "l̩") "əl",
      Replace (s "m̩") "əm",
      Replace (s "u̯") "u",
      Replace (s "ʊ̯") "ʊ",
      Replace (s "ɪ̯") "ɪ",
      Replace (s "ɨ") "ɪ",
      Replace (s "ɾ̃") "d",
      Replace (s "ː") "",
      Replace (s "ʁ") "", -- alternatively, this could possibly be syllablized as ə
      Replace (s "ɛɹ") "ɛ.ɹ", -- sylbreak avoids confusion, see [1]
      Replace (s "(ɹ)") "ɹ",
      Replace (s "(t)") "t",
      Replace (s "/") "",
      Replace (s "ʔ") "",
      Replace (s "(") "", -- remove all parens
      Replace (s ")") "",
      Replace (s "-") "",
      Replace (s "æɹ") "ɛɹ", -- from Wiktionary's pronunciation guide, see [2]
      Replace (s "ɛɪ") "eɪ",
      Replace (s "əʊ") "oʊ",
      Replace (s "ɜː") "ɜɹ"
    ]

replSnd :: Trie
replSnd =
  listToTrie
    [ Replace (s "əɪ") "eɪ", -- reversing the e swap above [a]
      Replace (s "ɛ˞") "ɜ˞", -- reversing the epsilon swap above [b]
      Replace (s "æɪ") "aɪ", -- reversing the ash swap above [c]
      Replace (s "æu") "aʊ", -- [c]
      Replace (s "æʊ") "aʊ", -- [c]
      Replace (s "ɛɹ") "ɜ˞",
      Replace (s "au") "aʊ" -- old replacement, should be redundant, see [c]
    ]

replThrd :: Trie
replThrd =
  listToTrie
    [ Replace (s "oʊ") "o͡ʊ",
      Replace (s "eɪ") "e͡ɪ",
      Replace (s "aɪ") "a͡ɪ", -- note some ligatures render a+͡ as ɑ, it is a+͡
      Replace (s "ɔɪ") "ɔ͡ɪ",
      Replace (s "aʊ") "a͡ʊ",
      Replace (s "ɚ") "ə˞",
      Replace (s "ɝ") "ɜ˞",
      Replace (s "ɜɹ") "ɜ˞",
      Replace (s "əɹ") "ə˞",
      Replace (s "tʃ") "t͡ʃ",
      Replace (s "dʒ") "d͡ʒ"
    ]

replNonChar :: Trie
replNonChar =
  listToTrie
    [ Replace (s " ") "",
      Replace (s "\r") "",
      Replace (s "\t") "",
      Replace (s "\n") ""
    ]

s :: String -> String'
s = string'fromString

-- Note [1]: in the normalizing of r-colored vowels, it's possible to overreach
-- and affect ɹ's on the other side of a syllable divide (ex. derivate :
-- /dɛɹɪveɪt/ -> dɛ.ɹɪ.veɪt, not dɛ(ɹ).ɪ.veɪt).
-- Note [2]: see https://en.wiktionary.org/wiki/Appendix:English_pronunciation

stressSymbols :: [String]
stressSymbols = T.unpack <$> [stressSymbolIPA, secondaryStressSymbolIPA]

syllableBreakSymbol :: String
syllableBreakSymbol = "."

ipaSymbols :: [String]
ipaSymbols =
  ( sortOn (Down . length)
      . (++) (syllableBreakSymbol : stressSymbols)
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
