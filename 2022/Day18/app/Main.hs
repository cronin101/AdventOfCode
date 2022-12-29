module Main (main) where

import Lib (exteriorSurfaceArea, loadInput, surfaceArea)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ surfaceArea input
  print $ exteriorSurfaceArea input
