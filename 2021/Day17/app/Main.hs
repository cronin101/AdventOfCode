module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ findMaxHeight . snd $ input
  print $ length $ validVelocities input
