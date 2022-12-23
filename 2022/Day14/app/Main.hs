module Main (main) where

import Lib (addFloor, loadInput, sandCount, showWorld, solve)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  putStrLn $ showWorld input
  let solved = solve input
  putStrLn $ showWorld solved
  print $ sandCount solved

  let floorInput = addFloor input
  putStrLn $ showWorld floorInput
  let floorSolved = solve floorInput
  putStrLn $ showWorld floorSolved
  print $ sandCount floorSolved
