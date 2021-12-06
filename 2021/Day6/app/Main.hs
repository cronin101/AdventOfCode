module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ fishCount $ afterDays 80 input
  print $ fishCount $ afterDays 256 input
