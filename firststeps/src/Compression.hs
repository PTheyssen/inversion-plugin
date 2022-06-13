{-# OPTIONS_GHC -fplugin Plugin.InversionPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Compression where

import Prelude

-- Run length encoding as first example of a compression algorithm
-- for simplicity assume integers as data
runLengthEncoder :: [Int] -> [Int]
runLengthEncoder xs = foldl (++) [] $ zipWith (\x y -> x : y : []) digits times
  where
    s = splitList xs
    digits = map head s
    times = map length s

-- take original list, split it whenever digit changes, in the end just length
-- of each of these lists
splitList :: [Int] -> [[Int]]
splitList [] = []
splitList [x] = [[x]]
splitList l@(x:_) = (takeWhile eqX l) : (splitList (dropWhile eqX l))
  where
    eqX = (\y -> x == y)
