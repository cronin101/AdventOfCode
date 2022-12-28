module Main (main) where

import Lib (heightAtScore, loadInput, loadSprites, prepareSolutionBStart, startState)

main :: IO ()
main = do
  sprites <- loadSprites
  input <- loadInput "input.txt"
  let state = startState sprites input
  print $ heightAtScore 2022 state
  let solutionBStart = prepareSolutionBStart (length input * 5) state
  print $ heightAtScore 1000000000000 solutionBStart
