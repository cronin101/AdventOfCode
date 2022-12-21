module Main (main) where

import Lib (loadInput, printExecution, runProgram, signalStrengthSum)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ signalStrengthSum $ runProgram input
  putStr $ printExecution $ runProgram input