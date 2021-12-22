module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let solved = solve input
  print $ knownBeaconCount solved
  print $ largestDistance solved
