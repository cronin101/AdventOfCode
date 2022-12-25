module Main (main) where

import Lib (loadInput, solutionA)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ solutionA input
