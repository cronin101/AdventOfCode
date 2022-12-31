module Main (main) where

import Lib (evaluate, loadInput, search, setRootAndHumn)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ evaluate "root" input
  let aSolution = search input (minBound :: Int, maxBound :: Int)
  -- There's actually multiple correct answers (because of modulo arithmetic? and the puzzle only accepts the lowest one)
  print $ head $ dropWhile ((/= 0) . evaluate "root" . flip setRootAndHumn input) [aSolution - 10 .. aSolution + 10]