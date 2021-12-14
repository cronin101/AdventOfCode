module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ quantityDelta $ stepN 10 $ input
  print $ quantityDelta $ stepN 40 $ input