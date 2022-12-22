module Main (main) where

import Lib (Monkey (getsBored), loadInput, monkeyBusiness, monkeyChain)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let chain = monkeyChain 20 input
  print $ monkeyBusiness chain

  let harderInput = map (\monkey -> monkey {getsBored = False}) input
  let harderChain = monkeyChain 10000 harderInput
  print $ monkeyBusiness harderChain
