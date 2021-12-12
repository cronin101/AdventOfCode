module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ length . caveTraversals $ input
  print $ length . allTraversalsWithRevisit $ input
