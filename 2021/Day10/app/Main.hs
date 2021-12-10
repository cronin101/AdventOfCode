module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let checked = map syntaxCheck input
  print $ sum $ map scoreCorruptedSyntaxCheckState $ filter isCorrupted checked
  print $ takeMiddle $ map scoreIncompleteSyntaxCheckState $ filter isIncomplete checked
