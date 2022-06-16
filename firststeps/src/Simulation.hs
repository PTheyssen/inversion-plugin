{-# OPTIONS_GHC -fplugin Plugin.InversionPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Simulation where

import Prelude hiding ((++), lookup, Maybe(..), length)

--------------------------------------------------------------------------------
-- Implementing a simple simulation of free falling objects
--------------------------------------------------------------------------------

data Height = Height Int
  deriving (Show)
data Velocity = Velocity Int
  deriving (Show)
data Time = Time Int
  deriving (Show)
data TimeEnd = TimeEnd Int
  deriving (Show)

freeFall :: (Height, Velocity, Time, TimeEnd) -> (Height, Velocity, Time, TimeEnd)
freeFall current@((Height h), (Velocity v), (Time t), (TimeEnd tEnd)) =
  case t == tEnd of
    True -> current
    False -> freeFall ((Height h'), (Velocity v'), (Time t'), (TimeEnd tEnd))
      where
        v' = v + 10
        h' = h - v' + 5
        t' = t + 1
