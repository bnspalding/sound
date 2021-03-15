# TODO

- move away from using Sound and start using Phone to describe bits of speech

- a Sound is not a sound, it's actually a particular symbolic representation
of a phoneme. It's useful to know what kind of representation you're dealing with
(IPA vs XSAMPA for example), so change the Sound type to easily expand to other
types than IPA
data Phoneme = IPA T.Text

- write a note on phones vs phonemes, phonemic vs phonetic transcription

- review features and feature structure

- syllabification is tied to accent and should be kicked out of the main library

- Phonemes should not derive Ord. Look at using unordered-containers for phoneme
  sets
