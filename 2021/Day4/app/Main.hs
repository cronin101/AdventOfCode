module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let Just (_, firstScore, _) = playBingo input
  print firstScore
  let Just (_, lastScore, _) = playUntilLastWin input
  print lastScore
