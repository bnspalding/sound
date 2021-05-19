-- |
-- Module: Sound.Accents.Builders.Consonants
-- Description: segment builders for consonants
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- This module contains builders for constructing different types of consonants.
-- The structure of builders is meant to mirror the way that phonemes are
-- defined in natural language. For example, \/p\/ is a "voiceless (vl) labial
-- stop".
module Sound.Accents.Builders.Consonants
  ( vd,
    vl,
    stop,
    nasal,
    fricative,
    glide,
    approximant,
    distrib,
    sibilant,
    lateral,
    bilabial,
    labiodental,
    alveolar,
    dental,
    postalveolar,
    velar,
    palatal,
    glottal,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Sound.Feature hiding (distrib, lateral, nasal)
import qualified Sound.Feature as F (distrib, lateral, nasal)
import Prelude hiding (round)

aBase :: AutosegmentalFeatures
aBase =
  AutosegmentalFeatures
    { F.nasal = Nothing,
      F.lateral = Nothing,
      strident = Nothing,
      continuant = Nothing,
      place = pBase,
      laryngeal = Nothing
    }

pBase :: Place
pBase =
  Place
    { labial = Nothing,
      coronal = Nothing,
      dorsal = Nothing,
      pharyngeal = Nothing
    }

-- | a voiced segment
vd :: Segment -> Segment
vd seg =
  seg
    { autosegmentalFeatures =
        aFeats
          { laryngeal =
              Just lFeats {voice = Just Plus}
          }
    }
  where
    aFeats = autosegmentalFeatures seg
    lFeats = fromMaybe (LaryngealFeatures Nothing Nothing Nothing) (laryngeal aFeats)

-- | a voiceless segment
vl :: Segment -> Segment
vl seg =
  seg
    { autosegmentalFeatures =
        aFeats
          { laryngeal =
              Just lFeats {voice = Just Minus}
          }
    }
  where
    aFeats = autosegmentalFeatures seg
    lFeats = fromMaybe (LaryngealFeatures Nothing Nothing Nothing) (laryngeal aFeats)

-- | a consonant that is (-continuant, -sonorant)
stop :: T.Text -> Segment
stop sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Plus,
            sonorant = Minus,
            syllabic = Minus
          },
      autosegmentalFeatures =
        aBase
          { continuant = Just Minus,
            place = pBase
          },
      symbol = sym
    }

-- | a consonant that is (+sonorant, -continuant, nasal)
nasal :: T.Text -> Segment
nasal sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Plus,
            sonorant = Plus,
            syllabic = Minus
          },
      autosegmentalFeatures =
        aBase
          { F.nasal = Just Marked,
            continuant = Just Minus,
            place = pBase
          },
      symbol = sym
    }

-- | a consonant that is (-sonorant, +continuant, -strident)
fricative :: T.Text -> Segment
fricative sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Plus,
            sonorant = Minus,
            syllabic = Minus
          },
      autosegmentalFeatures =
        aBase
          { continuant = Just Plus,
            strident = Just Minus,
            place = pBase
          },
      symbol = sym
    }

-- | a semivowel (-consonantal, +sonorant, -syllabic, +continuant)
glide :: T.Text -> Segment
glide sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Minus,
            sonorant = Plus,
            syllabic = Minus
          },
      autosegmentalFeatures =
        aBase
          { continuant = Just Plus
          },
      symbol = sym
    }

-- | a consonant that is (+sonorant, +continuant)
approximant :: T.Text -> Segment
approximant sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Plus,
            sonorant = Plus,
            syllabic = Minus
          },
      autosegmentalFeatures =
        aBase {continuant = Just Plus},
      symbol = sym
    }

-- | a segment marked +strident
sibilant :: Segment -> Segment
sibilant seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { strident = Just Plus
          }
    }

-- | a segment marked +distrib
distrib :: Segment -> Segment
distrib seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { coronal =
                    Just
                      corFeats
                        { F.distrib = Just Plus
                        }
                }
          }
    }
  where
    corFeats =
      fromMaybe
        (CoronalFeatures Nothing Nothing)
        (coronal (place (autosegmentalFeatures seg)))

-- | a segment marked lateral
lateral :: Segment -> Segment
lateral seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { F.lateral = Just Marked
          }
    }

-- | a labially articulated segment
bilabial :: Segment -> Segment
bilabial seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { labial = Just LabialFeatures {round = Nothing}
                }
          }
    }

-- | a labially articulated segment. From the perspective of distinctive
-- features, this is marked the same as 'bilabial'.
labiodental :: Segment -> Segment
labiodental = bilabial

-- | a coronally articulated segment marked (+anterior)
alveolar :: Segment -> Segment
alveolar seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { coronal =
                    Just
                      CoronalFeatures
                        { anterior = Just Plus,
                          F.distrib = Just Minus
                        }
                }
          }
    }

-- | a coronally articulated segment marked (+anterior). From the perspective of 
-- distinctive features, this is marked the same as 'alveolar'.
dental :: Segment -> Segment
dental = alveolar

-- | a coronally articulated segment marked (-anterior)
postalveolar :: Segment -> Segment
postalveolar seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { coronal =
                    Just
                      CoronalFeatures
                        { anterior = Just Minus,
                          F.distrib = Just Minus
                        }
                }
          }
    }

-- | a dorsally articulated segment.
velar :: Segment -> Segment
velar seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      DorsalFeatures
                        { high = Nothing,
                          low = Nothing,
                          back = Nothing
                        }
                }
          }
    }

-- | a dorsally articulated segment. From the perspective of distinctive
-- features, this is marked the same as 'velar'.
palatal :: Segment -> Segment
palatal = velar

-- | a segment with laryngeal constriction.
glottal :: Segment -> Segment
glottal seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { laryngeal =
              Just lFeats
          }
    }
  where
    lFeats =
      fromMaybe
        (LaryngealFeatures Nothing Nothing Nothing)
        (laryngeal (autosegmentalFeatures seg))
