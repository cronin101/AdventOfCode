module Main (main) where

import Lib (loadInput, markSquares, part1, part2, printInput)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ part1 input

  print $ part2 input

-- putStrLn $ printInput $ markSquares input
