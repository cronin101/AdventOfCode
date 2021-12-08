module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ sum $ map countEasyDigitsInOutput input
  print $ sum $ map (snd . convertInput) input
