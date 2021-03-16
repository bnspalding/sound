# TODO

- move away from using Sound and start using Phone to describe bits of speech

- instead of using a general Sound (or Phone) type, use a Phoneme class that any
  accent can implement. Write the sound library generally in terms of Phonemes.
  Phoneme requires a way to map the object to a feature set, and a symbolic
  representation for the phoneme.
  An accent implements Phoneme on the type it exports, and then sound works on
  that concrete type

- Show is for labeling an object, symbol is for its textual representation.
  ex: a GenAm Phoneme {symbol: "θ", show: "vl dental fricative"}
  This is important also for consistency with Stress, where not every object has
  a symbolic representation (Unstressed)

- write a note on phones vs phonemes, phonemic vs phonetic transcription

- review features and feature structure

- syllabification is tied to accent and should be kicked out of the main library

- Phonemes should not derive Ord. Look at using unordered-containers for phoneme
  sets

- Get rid of Pronunciation.hs. it's not exposing anything of particular value
  and is tied to GenAm in an unnecessary way.

- the current implementation of Features and FeatureSets permits contradictory
  features, where a set may contain both PLUS_X and MINUS_X at the same time.
  There's an opportunity to revisit Features in a productive way here.

- Sound/Accents/GenAM/IPA.hs is a weird file. I don't think the
  replacements/reductions that it is performing are necessary, and I don't like
  the way that it goes about it. Consider how much of this can just be removed.

- The GenAm accent should export a type, not a map. (see shift to Phoneme class)
