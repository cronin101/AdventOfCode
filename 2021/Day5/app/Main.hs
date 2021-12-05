module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let cardinalLines = filter isCardinalDirection input
  print $ length $ pointsTraversedMoreThanOnce $ cardinalLines
  print $ length $ pointsTraversedMoreThanOnce $ input
