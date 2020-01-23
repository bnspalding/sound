module Sound.Sound where

newtype Sound
  = Sound String
  deriving (Eq, Show, Ord)
-- TODO: change out Strings for T.Text across package
