module Main (main) where

import Lib
import Visualize

main :: IO ()
main = do
  input <- loadInput "input.txt"
  putStr $ toDot input
