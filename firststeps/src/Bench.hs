module Bench where

import Criterion.Main
import Criterion.Types

import Main hiding ( main )

main :: IO ()
main = do
  let xs = 0 : map (+5) xs :: [Int]
  let ys = 10 : map (+10) ys :: [Int]
  putStrLn "Benchmarking"
  defaultMainWith
    (defaultConfig {reportFile = Just "benchmarks.html"}) $
    [bgroup "removeAt1" [ bench "4 [1,2,3,4,5,6]"
                          $ nf (\x -> take 1 (removeAt1 4 x)) [1,2,3,4,5,6],
                          bench "0 [3,2,1]"
                          $ nf (\x -> take 1 (removeAt1 0 x)) [3,2,1]
                        ]
    ]
