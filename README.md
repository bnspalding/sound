# Sound

Sound contains a collection of tools for working with symbolic representations
of speech sounds in Haskell. This includes:

- phonological units of sound (phoneme, syllable, word)
- [phonological features](https://en.wikipedia.org/wiki/Distinctive_feature)
- [IPA symbols](https://en.wikipedia.org/wiki/International_Phonetic_Alphabet)
- [stress](https://en.wikipedia.org/wiki/Stress_(linguistics))

I'm using this library for computer generated poetry. The Meter and Rhyme
modules provide tools for evaluating syllables based on their linguistic
features.

## Accents

This package is being developed with the [General American English 
accent](https://en.wikipedia.org/wiki/General_American_English) as its
primary use case, but I'm trying to ensure that the GenAm components of the
package are decoupled in a way that promotes modularity. My intent is to make
the package receptive to other accents and symbol-feature mappings. Some
features of the package, such as automatic syllabification, are currently tied
to the GenAm accent. Future work will involve a full decouple and a clearer
structure for passing different accents to the rest of the package.

The package treats the General American English accent as its primary use case
because it is close to my (the author's) accent and it is generally familiar for
the type of poetry that I'm producing with this package. It is an abstraction
loosely capturing only a part of the variety of American English accents and it
should not be treated as exceptional, general, or as a standard among other
accents.

## Usage

### convert IPA symbols into sounds

The symbols that represent a sound are stored in a Sound structure. Because a
Sound is just a structural wrapping for its symbols, a Sound can be constructed
simply.

```haskell
import Sound (Sound)

Sound "h"
-- The sound "h"

Sound <$> ["h", "ɛ", "l", "o͡ʊ"]
-- Sound mapped over a list of symbols
```

To ensure that the symbols being used are valid within an accent, use
`stringToIPASounds` from the `Sound.Accents.GenAm.IPA` module. The GenAm accent
recognizes and defines feature information for only a subset of all IPA symbols,
and `stringToIPASounds` returns a `Maybe` value to reflect this. Additionally, 
this function performs some reductions and transformations on the broader set of
IPA symbols to constrain them to the smaller, regular subset that it knows.

```haskell
import qualified Sound.Accents.GenAm.IPA as GenAm

GenAm.stringToIPASounds "hɛlo͡ʊ"
-- Just [Sound "h", Sound "ɛ", Sound "l", Sound "o͡ʊ"]

GenAm.stringToIPASounds "helo"
-- Just [Sound "h", Sound "ə", Sound "l", Sound "o͡ʊ"]
-- e and o are transformed, but note the first vowel is not the same as above

GenAm.stringToIPASounds "h2lo"
-- Nothing; after transformations, there are still unknown symbols in the string
```

The set of Sounds that comprise the GenAm symbol set is available as `sounds`.
This set does not consider any transformations that would be applied using
`stringToIPASounds`, and contains just the symbols of the accent.

```haskell
import qualified Sound.Accents.GenAm as GenAm
import Sound (Sound)
import qualified Data.Set as Set

(Sound "t") `Set.member` GenAm.sounds
-- True

(Sound "e") `Set.member` GenAm.sounds
-- False
```

### represent sounds as sets of phonological features

Using a particular accent, sounds can be looked up in a map of phonological
features. These features are atomic properties that describe the utterance of a
sound, and they can be used to classify and reason about sounds.

The result of `features` is wrapped in a `Maybe` because, again, not all symbols
are valid symbols within a particular accent.

```haskell
import qualified Sound.Accents.GenAm as GenAm
import Sound (Sound)
import qualified Data.Set as Set

Set.toList <$> GenAm.features (Sound "t")
-- Just [
--  MINUS_SYLLABIC,PLUS_CONSONANTAL,MINUS_SONORANT,
--  MINUS_CONTINUANT,MINUS_VOICE,CORONAL,PLUS_ANTERIOR,MINUS_DISTRIB
-- ]
```

`Sound.Features` provides tools for classifying sounds according to these
features.

```haskell
import Sound.Feature (isStop, isVowel, isVoiced)

let Just (featuresOfB) = GenAm.features (Sound "b")

isStop featuresOfB
-- True

isVowel featuresOfB
-- False

isVoiced featuresOfB
-- True
```

### automatically syllabify words (GenAm only)

When you do not know the location of syllable breaks for a word,
`Sound.Syllabify` can take a guess for you. It attempts to find syllable breaks
by using the differences in sonority between sounds. In its current state, it
works well-ish and is tied to the GenAm accent.

```haskell
import Sound.Syllabify (syllabify)
import Sound.Word (symbols)
import Sound (Sound)

symbols $ syllabify (Sound <$> ["s", "k", "w", "ɜ˞", "ə", "l"])
-- "skwɜ˞.əl"
```

When stress information is not provided, `syllabify` labels the syllable as
`Unstressed`. However, you can provide stress information as symbols in your
list of Sounds. Use the IPA symbols "ˈ" (Unicode U+02C8) and "ˌ" (Unicode
U+02CC) to mark stress and secondary stress respectively. The symbols "ə" and
"ə˞" are always marked as reduced stress. `Sound.Stress` provides functions for
moving from a system of four stress levels (stress, secondary stress,
unstressed, reduced stress) to a binary system of stress (high, low).

```haskell
import Sound.Syl (stress)

symbols $ syllabify (Sound <$> ["ˈ", "s", "k", "w", "ɜ˞", "ə", "l"])
-- "ˈskwɜ˞.əl"

stress <$> syllabify (Sound <$> ["ˈ", "s", "k", "w", "ɜ˞", "ə", "l"])
-- [Just Stressed, Just ReducedStress]
```

Syllabification also accepts the syllable break symbol "." (a period). It will
automatically break at these points when constructing syllables. Syllabification
is most useful as a constructor for syllables when you have stress and break 
information, because it splits sounds internally (onset, nucleus, coda) without
having to guess where syllable breaks are.

### measure rhyme, assonance, alliteration

Rhyme provides tools for comparing syllables for purposes of rhyme,
assonance, and alliteration.

`Rhyme.Strict` simply compares symbols between syllables. 

```haskell
import Rhyme.Strict (rhyme, assonance, alliteration)
import Sound (Sound)
import Sound.Syllabify (syllabify)

let tack = head . syllabify $ Sound <$>  ["t", "æ", "k"]
let stack = head . syllabify $ Sound <$> ["s", "t", "æ", "k"]
let stark =  head . syllabify $ Sound <$>  ["s", "t", "ɑ", "ɹ", "k"]

rhyme tack stack
-- True

rhyme stack stark
-- False

assonance tack stack
-- True

alliteration stack stark
-- True
```

`Rhyme.Approx` compares syllables by similarity of feature sets. It returns a
ratio that is (_shared features in a set of sounds_) / (_total features in a set
of sounds_).

```haskell
import Rhyme.Approx (rhyme, assonance, alliteration)

-- using the words defined in the example above...
-- note that the fractions are reduced and not the actual number of features

rhyme tack stack
-- 1 % 1; similarity = 1

rhyme stack stark
-- 7 % 10; similarity = 0.7

alliteration tack stack
-- 4 % 5; similarity = 0.8
```

## More Documentation

Module documentation can be generated for the package using haddock.
Run `stack haddock --open sound` to view the package once it has been included
in your project.

## License

See the [LICENSE](https://github.com/bnspalding/sound/blob/master/LICENSE) file in the repository.
