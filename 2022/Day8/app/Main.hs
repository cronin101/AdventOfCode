module Main (main) where

import Lib (bestScore, countVisible, loadInput, solve)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let solved@(bCMap, kMap, _) = solve input
  print $ countVisible solved
  let scenicScore = bestScore solved
  print scenicScore
