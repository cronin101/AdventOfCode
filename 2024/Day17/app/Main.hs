module Main (main) where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ part1 input
  print $ explain input
  print $ part2 input