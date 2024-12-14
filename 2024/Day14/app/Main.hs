module Main (main) where

import Lib

main :: IO ()
main = do
  input <- loadInput (101, 103) "input.txt"
  print $ part1 input
  print $ part2 input