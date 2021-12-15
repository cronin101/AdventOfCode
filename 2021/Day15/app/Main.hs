module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ exitCost input
  print $ exitCost $ expandLandscape input
