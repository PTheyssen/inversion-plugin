{-# LANGUAGE TemplateHaskell, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Plugin.InversionPlugin

import Simulation
import Compression
import PartialInverses
import Criterion.Main
import Criterion.Types

import Prelude hiding (map, lookup, (++), last)

main :: IO ()
main = do
  putStrLn "Benchmarking automatically generated Inverses"
  defaultMainWith
    (defaultConfig {reportFile = Just "benchmarks.html",
                    csvFile = Just "benchmark-inverses.csv"}) $
    [bgroup "removeAt1" [ bench "4 [1,2,3,4,5,6]"
                          $ nf (\x -> take 1 (removeAt1 4 x)) [1,2,3,4,5,6],
                          bench "0 [3,2,1]"
                          $ nf (\x -> take 1 (removeAt1 0 x)) [3,2,1],
                          bench "50 [0..300]"
                          $ nf (\x -> take 1 (removeAt1 50 x)) [0..300]
                        ],
     bgroup "removeAt1Manual" [ bench "4 [1,2,3,4,5,6]"
                                $ nf (\x -> removeAt1Manual 4 x) [1,2,3,4,5,6],
                                bench "0 [3,2,1]"
                                $ nf (\x -> removeAt1Manual 0 x) [3,2,1],
                                bench "50 [0..300]"
                                $ nf (\x -> removeAt1Manual 50 x) [0..300]
                        ],
     bgroup "runLengthDecoder" [ bench "[1,5]" $
                                 nf (\x -> take 1 (runLengthDecoder x)) [1,5],
                                 bench "[1,5,2,2,3,3]" $
                                 nf (\x -> take 1 (runLengthDecoder x)) [1,5,2,2,3,3]
                               ],
     bgroup "runLengthDecoderManual" [ bench "[1,5]" $
                                       nf (\x -> runLengthDecoderManual x) [1,5],
                                       bench "[1,5,2,2,3,3]" $
                                       nf (\x -> runLengthDecoderManual x)
                                       [1,5,2,2,3,3]
                                     ]
    ]


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

runLengthDecoder :: [Int] -> [[Int]]
runLengthDecoder = $(inv 'runLengthEncoder)

-- implemented for benchmarking manual vs automatic generated inverse
runLengthDecoderManual :: [Int] -> [Int]
runLengthDecoderManual [] = []
runLengthDecoderManual [x] = error "Invalid encoding"
runLengthDecoderManual (x:(y:ys)) = (take y $ repeat x) ++ runLengthDecoderManual ys

dataToCompress :: [Int]
dataToCompress = [1,1,1,1,1,1,1,2,2,3,3,3,5]

encoded :: [Int]
encoded =  runLengthEncoder dataToCompress

decoded :: [[Int]]
decoded = take 1 $ runLengthDecoder encoded

--------------------------------------------------------------------------------
-- Partial Inverses
--------------------------------------------------------------------------------

-- partial inverse of insertAt fixing the first argument (index)
removeAt1 :: Int -> [Int] -> [(Int, [Int])]
removeAt1 = $(partialInv 'insertAt [1])

-- implemented for benchmarking manual vs automatic generated inverse
removeAt1Manual :: Int -> [Int] -> (Int, [Int])
removeAt1Manual _ [] = error "empty list"
removeAt1Manual 0 (x:xs) = (x, xs)
removeAt1Manual i (x:xs) = removeAt1Manual (i-1) xs

removeAt1Example :: [(Int, [Int])]
removeAt1Example = (take 1 (removeAt1 3 [1,2,3,4,5]))
