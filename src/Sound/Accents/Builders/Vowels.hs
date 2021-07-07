-- |
-- Module: Sound.Accents.Builders.Vowels
-- Description: segment builders for vowels
-- Copyright: (c) 2019 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: experimental
--
-- Vowels contains builders for constructing different types of vowels. The
-- structure of builders are meant to mirror the way that phonemes are defined
-- in natural language. For example, \/æ\/ is a "near-open front unrounded
-- vowel"
module Sound.Accents.Builders.Vowels
  ( front,
    back,
    high,
    mid,
    low,
    rounded,
    unrounded,
    tense,
    rhotic,
    vowel,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Sound.Feature hiding (back, high, low, rhotic)
import qualified Sound.Feature as F (back, high, low, rhotic)
import Prelude hiding (round)

aBase :: AutosegmentalFeatures
aBase =
  AutosegmentalFeatures
    { nasal = Nothing,
      lateral = Nothing,
      F.rhotic = Nothing,
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
      pharyngeal = Just PharyngealFeatures {advancedTongueRoot = Just Minus}
    }

front :: Segment -> Segment
front seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      dorFeats
                        { F.back = Just Minus
                        }
                }
          }
    }
  where
    dorFeats =
      fromMaybe
        (DorsalFeatures Nothing Nothing Nothing)
        (dorsal (place (autosegmentalFeatures seg)))

back :: Segment -> Segment
back seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      dorFeats
                        { F.back = Just Plus
                        }
                }
          }
    }
  where
    dorFeats =
      fromMaybe
        (DorsalFeatures Nothing Nothing Nothing)
        (dorsal (place (autosegmentalFeatures seg)))

high :: Segment -> Segment
high seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      dorFeats
                        { F.high = Just Plus,
                          F.low = Just Minus
                        }
                }
          }
    }
  where
    dorFeats =
      fromMaybe
        (DorsalFeatures Nothing Nothing Nothing)
        (dorsal (place (autosegmentalFeatures seg)))

mid :: Segment -> Segment
mid seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      dorFeats
                        { F.high = Just Minus,
                          F.low = Just Minus
                        }
                }
          }
    }
  where
    dorFeats =
      fromMaybe
        (DorsalFeatures Nothing Nothing Nothing)
        (dorsal (place (autosegmentalFeatures seg)))

low :: Segment -> Segment
low seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { dorsal =
                    Just
                      dorFeats
                        { F.high = Just Minus,
                          F.low = Just Plus
                        }
                }
          }
    }
  where
    dorFeats =
      fromMaybe
        (DorsalFeatures Nothing Nothing Nothing)
        (dorsal (place (autosegmentalFeatures seg)))

rounded :: Segment -> Segment
rounded seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { labial =
                    Just
                      labFeats
                        { round = Just Marked
                        }
                }
          }
    }
  where
    labFeats =
      fromMaybe
        (LabialFeatures Nothing)
        (labial (place (autosegmentalFeatures seg)))

unrounded :: Segment -> Segment
unrounded seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { labial = Nothing
                }
          }
    }

tense :: Segment -> Segment
tense seg =
  seg
    { autosegmentalFeatures =
        (autosegmentalFeatures seg)
          { place =
              (place (autosegmentalFeatures seg))
                { pharyngeal =
                    Just
                      pharFeats
                        { advancedTongueRoot = Just Plus
                        }
                }
          }
    }
  where
    pharFeats =
      fromMaybe
        (PharyngealFeatures Nothing)
        (pharyngeal (place (autosegmentalFeatures seg)))

rhotic :: Segment -> Segment
rhotic seg =
  seg
    { autosegmentalFeatures =
      (autosegmentalFeatures seg)
        { F.rhotic = Just Marked }
        }

vowel :: T.Text -> Segment
vowel sym =
  Segment
    { rootFeatures =
        RootFeatures
          { consonantal = Minus,
            sonorant = Plus,
            syllabic = Plus
          },
      autosegmentalFeatures =
        aBase
          { continuant = Just Plus
          },
      symbol = sym
    }
