module Main where

import Lib (loadInput, magnitude', reduceMany)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ magnitude' $ reduceMany input
  print $ maximum [magnitude' $ reduceMany [fish1, fish2] | fish1 <- input, fish2 <- input, fish1 /= fish2]