module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ riskLevel input
  print $ biggestBasinProduct input
