module Main (main) where

import           Lib (evaluateStrategy, loadInputAsStrategyAEntries,
                      loadInputAsStrategyBEntries, scoreRound)

main :: IO ()
main = do
  strategyAEntries <- loadInputAsStrategyAEntries "input1.txt"
  let scoreA = sum $ map scoreRound strategyAEntries
  print scoreA

  strategyBEntries <- loadInputAsStrategyBEntries "input1.txt"
  let scoreB = sum $ map (scoreRound . evaluateStrategy) strategyBEntries
  print scoreB


