module Main (main) where

import Lib (loadInput, part1, part2)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ part1 input
  print $ part2 input
