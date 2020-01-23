-- |
-- Module: N
-- Description: Short Description
-- Copyright: (c) 2020 Ben Spalding (bnspalding.com)
-- License: MIT
-- Stability: Experimental
--
-- Longer description
module Sound.Sound where

newtype Sound
  = Sound String
  deriving (Eq, Show, Ord)
-- TODO: change out Strings for T.Text across package
