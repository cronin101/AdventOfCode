module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "virtualExample.txt"
  print $ virtualVolume $ virtualSimulate $ take 25 $ input
