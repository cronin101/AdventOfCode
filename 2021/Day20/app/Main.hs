module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let enhanced = enhance input
  print $ show $ pixelCount enhanced
