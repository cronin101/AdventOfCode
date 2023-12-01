module Main (main) where

import Lib (loadInput)

main :: IO ()
main = do
  partOneInput <- loadInput False "input.txt"
  print $ sum partOneInput

  partTwoInput <- loadInput True "input.txt"
  print $ sum partTwoInput
