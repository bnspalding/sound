# Sound

Sound contains a collection of tools for working with symbolic representations
of sounds in Haskell. This includes:

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

- convert IPA symbols into sounds using Sound.GenAm.IPA
- represent sounds as sets of phonological features using Sound.Feature
- automatically syllabify words using Sound.Syllabify
- measure rhyme, assonance, alliteration (including approximate measures) using
  Sound.Rhyme

## More Documentation

Module documentation can be generated for the package using haddock.
Run `stack haddock --open sound` to view the package once it has been included
in your project.

## License

See the [LICENSE](https://github.com/bnspalding/sound/blob/master/LICENSE) file in the repository.
