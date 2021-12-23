module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ computeOutput $ playUntilWinner $ input
  print $ mostWins $ playQuantum $ toQuantumState $ input
