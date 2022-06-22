{-# LANGUAGE TemplateHaskell, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Plugin.InversionPlugin

import Simulation
import Compression
import PartialInverses

import Prelude hiding (map, lookup, (++), last, Maybe(..))


main :: IO ()
main = do
  putStr $ "Free fall simulation: " ++ (show fallDown) ++ "\n"
  putStr $ "First 5 inverses found : " ++ (show fallUp) ++ "\n"
  putStr $ "Compressed data : " ++ (show encoded) ++ "\n"
  putStr $ "Decompressed data : " ++ (show decoded) ++ "\n"
  putStr $ "removeAt1 3 [1,2,3,4,5] : " ++ (show removeAt1Example) ++ "\n"


split :: [Int] -> [([Int], [Int])]
split = $(inv '(++))

--------------------------------------------------------------------------------
-- Simulations
--------------------------------------------------------------------------------

freeFallInv :: (Height, Velocity, Time, TimeEnd)
             -> [(Height, Velocity, Time, TimeEnd)]
freeFallInv = $(inv 'freeFall)

fallStart :: (Height, Velocity, Time, TimeEnd)
fallStart = ((Height 176), (Velocity 0), (Time 0), (TimeEnd 3))

fallDown :: (Height, Velocity, Time, TimeEnd)
fallDown = freeFall fallStart

fallUp :: [(Height, Velocity, Time, TimeEnd)]
fallUp = take 5 $ freeFallInv fallDown

--------------------------------------------------------------------------------
-- Compressions
--------------------------------------------------------------------------------

runLengthEncoderInv :: [Int] -> [[Int]]
runLengthEncoderInv = $(inv 'runLengthEncoder)

dataToCompress :: [Int]
dataToCompress = [1,1,1,1,1,1,1,2,2,3,3,3,5]

encoded :: [Int]
encoded =  runLengthEncoder dataToCompress

decoded :: [[Int]]
decoded = take 1 $ runLengthEncoderInv encoded

--------------------------------------------------------------------------------
-- Partial Inverses
--------------------------------------------------------------------------------

-- partial inverse of insertAt fixing the first argument (index)
removeAt1 :: Int -> [Int] -> [(Int, [Int])]
removeAt1 = $(partialInv 'insertAt [1])

removeAt1Example :: [(Int, [Int])]
removeAt1Example = (take 1 (removeAt1 3 [1,2,3,4,5]))
