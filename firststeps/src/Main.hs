{-# LANGUAGE TemplateHaskell, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Plugin.InversionPlugin

import Inverses
import Compression

import Prelude hiding (map, lookup, (++), last, Maybe(..))


main :: IO ()
main = do
  putStr $ show split123

split123 :: [([Int], [Int])]
split123 = $(inv '(++)) [1,2,3]

freeFallInv :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
freeFallInv = $(inv 'freeFall)

-- *Main> freeFallStep (176, 0, 0, 3)
-- (131,30,3,3)

runLengthEncoderInv :: [Int] -> [[Int]]
runLengthEncoderInv = $(inv 'runLengthEncoder)

