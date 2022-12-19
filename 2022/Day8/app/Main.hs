module Main (main) where

import Lib (countVisible, loadInput, solve)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let solved = solve input
  print $ countVisible solved
