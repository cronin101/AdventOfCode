module Main (main) where

import Lib (countNoBeacon, findBeacon, loadInput, showWorld)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  -- putStrLn $ showWorld input
  print $ countNoBeacon input 2000000
  let (x, y) = findBeacon (0, 4000000) input
  print $ (x * 4000000) + y
