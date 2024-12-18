module Main (main) where

import Lib

main :: IO ()
main = do
  input <- loadInput (70, 70) "input.txt"
  print $ part1 1024 input
  print $ part2 input
