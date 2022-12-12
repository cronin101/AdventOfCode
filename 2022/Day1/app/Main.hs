module Main (main) where

import Lib (loadInput, sumInventoriesInPQueue, take)

main :: IO ()
main =
  do
    input <- loadInput "input1.txt"
    let summedInventories = sumInventoriesInPQueue input
    let topThree = map fst $ Lib.take 3 summedInventories
    print $ head topThree
    print $ sum topThree
