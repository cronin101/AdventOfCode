module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input1.txt"
  print $ countIncreases input
  print $ countWindowIncreases input
