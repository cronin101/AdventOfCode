module Main where

import Lib

main :: IO ()
main = do
  input@(pixels, instructions) <- loadInput "input.txt"
  print $ pixelCount $ followInstructions (pixels, take 1 instructions)
  putStrLn $ renderPixels $ followInstructions input
