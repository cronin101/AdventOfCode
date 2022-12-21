module Main (main) where

import Lib (loadInput, solutionA, solutionB)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ solutionA input
  print $ solutionB input