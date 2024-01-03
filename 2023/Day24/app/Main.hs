module Main (main) where

import Lib (loadInput, part1)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ part1 (200000000000000, 400000000000000) input
