{-# OPTIONS_GHC -fplugin Plugin.InversionPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module PartialInverses where

import Prelude hiding (length)

--------------------------------------------------------------------------------
-- Implement insertAt from which we will generate a partial inverse
-- removeAt. By fixing the index argument and giving a list, removeAt will
-- return the element at that index of the list and the remaining list
--------------------------------------------------------------------------------

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + (length xs)

insertAt :: Int -> Int -> [Int] -> [Int]
insertAt 0 x xs = (x:xs)
insertAt i x l@(y:ys) =
  case i < 0 of
    True -> error "invalid index"
    False ->
      case i > (length l) of
        True -> error "invalid index"
        False -> y : (insertAt (i-1) x ys)
