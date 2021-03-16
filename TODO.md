# TODO

- Show is for labeling an object, symbol is for its textual representation.
  ex: a GenAm Phoneme {symbol: "θ", show: "vl dental fricative"}
  This is important also for consistency with Stress, where not every object has
  a symbolic representation (Unstressed)

- write a note on phones vs phonemes, phonemic vs phonetic transcription

- Phonemes should not derive Ord. Look at using unordered-containers for phoneme
  sets

- the current implementation of Features and FeatureSets permits contradictory
  features, where a set may contain both PLUS_X and MINUS_X at the same time.
  There's an opportunity to revisit Features in a productive way here.

- Sound/Accents/GenAM/IPA.hs is a weird file. I don't think the
  replacements/reductions that it is performing are necessary, and I don't like
  the way that it goes about it. Consider how much of this can just be removed.
