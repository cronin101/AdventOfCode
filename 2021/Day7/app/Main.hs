module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let (_, linearCost) = cheapestPositionWithLinearCost input
  print linearCost
  let (_, geometricCost) = cheapestPositionWithGeometricCost input
  print geometricCost
