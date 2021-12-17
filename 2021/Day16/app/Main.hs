module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ versionSum input
  print $ value input
