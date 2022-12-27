module Main (main) where

import Lib (heightAtScore, hintSolutionB, loadInput, loadSprites, moveUpAndSetScore, startState)

main :: IO ()
main = do
  sprites <- loadSprites
  input <- loadInput "input.txt"
  let state = startState sprites input
  let modulo = lcm (length input) 5 -- 5 sprites per input
  print $ heightAtScore 2022 state
  print $ map ((\(h, s, _) -> (h, s)) . (\i -> hintSolutionB modulo i state)) [1 .. 5] -- 13081 and then 13080 delta y, 8574 and then 8575 delta score
  let lastCalculableHeightAndScore = last $ takeWhile ((<= 1000000000000) . snd) $ iterate (\(y, s) -> (y + 13080, s + 8575)) (13081, 8574)
  let solutionBStart = moveUpAndSetScore lastCalculableHeightAndScore $ (\(h, sc, st) -> st) $ hintSolutionB modulo 1 state
  print $ heightAtScore 1000000000000 solutionBStart
