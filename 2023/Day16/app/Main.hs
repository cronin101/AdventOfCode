module Main (main) where

import Lib (loadInput, part1, part2)

main :: IO ()
main = do
  grid <- loadInput "input.txt"
  print $ part1 grid
  print $ part2 grid
