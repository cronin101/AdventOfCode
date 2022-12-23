module Main (main) where

import Lib (correctIndices, decoderKey, loadInput)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ sum $ correctIndices input
  print $ decoderKey input
