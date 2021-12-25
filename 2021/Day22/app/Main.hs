module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ physicalVolume $ physicalSimulate $ input
  print $ virtualVolume $ virtualSimulate $ input
