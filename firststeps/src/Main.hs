{-# LANGUAGE TemplateHaskell, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Plugin.InversionPlugin

import Simulation
import Compression

import Prelude hiding (map, lookup, (++), last, Maybe(..))


main :: IO ()
main = do
  putStr $ "Free fall simulation: " ++ (show fallDown) ++ "\n"
  putStr $ "First 5 inverses found : " ++ (show fallUp) ++ "\n"
  putStr $ "Compressed data : " ++ (show encoded) ++ "\n"
  putStr $ "Decompressed data : " ++ (show decoded) ++ "\n"


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
dataToCompress = [1,1,1,1,1,1,1,2,2,0,0,0,3,3,3,5]

encoded :: [Int]
encoded =  runLengthEncoder dataToCompress

decoded :: [[Int]]
decoded = take 1 $ runLengthEncoderInv encoded

--------------------------------------------------------------------------------
-- Encryption
--------------------------------------------------------------------------------
