module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input1.txt"
  let (x, y) = sumDeltas input
  print $ x * y

  let (x', y') = calculatePositionWithAim input
  print $ x' * y'
