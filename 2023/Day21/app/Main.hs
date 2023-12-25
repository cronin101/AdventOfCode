module Main (main) where

import Lib (loadInput, part1, part2)

main :: IO ()
main =
  do
    input <- loadInput "input.txt"
    print $ part1 64 input
    print $ part2 input
