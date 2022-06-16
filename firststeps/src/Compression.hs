{-# OPTIONS_GHC -fplugin Plugin.InversionPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Compression where

import Prelude hiding ((++), map, takeWhile, dropWhile, length, head)

--------------------------------------------------------------------------------
-- Implementing a run length encoder, due to limitations in the inversion
-- package we cannot use functions from Prelude directly and instead must
-- implement them in this module
--------------------------------------------------------------------------------

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

head :: [a] -> a
head (x:_) = x
head [] = error "bad head"

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + (length xs)

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs) = case p x of
  True -> x : takeWhile p xs
  False -> []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs') = case p x of
  True -> dropWhile p xs'
  False -> xs

splitList :: [Int] -> [[Int]]
splitList [] = []
splitList [x] = [[x]]
splitList l@(x:_) = (takeWhile eqX l) : (splitList (dropWhile eqX l))
  where
    eqX = (\y -> x == y)

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] [] = []
mergeLists (x:xs) (y:ys) = [x,y] ++ (mergeLists xs ys)
mergeLists [] y = y
mergeLists x [] = x

runLengthEncoder :: [Int] -> [Int]
runLengthEncoder xs = mergeLists digits times
  where
    subLists = splitList xs
    digits = map head subLists
    times = map length subLists


-- TODO: implement Huffman Tree
