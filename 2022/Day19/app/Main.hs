module Main (main) where

import Lib (blueprintWithMaxGeodes, loadInput, startingStates)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print input
  print $ sum $ map (uncurry (*) . blueprintWithMaxGeodes) $ startingStates 24 input
  print $ product $ map (snd . blueprintWithMaxGeodes) $ take 3 $ startingStates 32 input
