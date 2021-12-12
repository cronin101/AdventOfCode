module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let (_, flashCount) = stepN (input, 0) 100
  print flashCount
  let Just syncFlashIndex = findSyncFlash input
  print syncFlashIndex
