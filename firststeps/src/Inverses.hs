{-# OPTIONS_GHC -fplugin Plugin.InversionPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Inverses where

import Prelude hiding ((++), lookup, Maybe(..))

-- type Height = Int
-- type Velocity = Int
-- type TimeEnd = Int
-- type Time = Int
-- type FallConfig = (Height, Velocity, Time, TimeEnd)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

is42 :: (Eq a, Num a) => a -> Bool
is42 42 = True
is42 _ = False


-- free fall function as basic example of physics simulation
freeFall :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
freeFall (h, v, t, tEnd) =
  case t == tEnd of
    True -> (h, v, t, tEnd)
    False -> freeFall (h', v', t', tEnd)
      where
        v' = v + 10
        h' = h - v' + 5
        t' = t + 1


-- freeFallStep :: FallConfig -> FallConfig
-- freeFallStep = undefined

-- freeFall :: Height -> TimeEnd -> Velocity
-- freeFall h tEnd =
  -- where

-- Run length encoding as first example of a compression algorithm
