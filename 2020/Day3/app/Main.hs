module Main where

import           Lib                            ( loadInput
                                                , treeCountForSlope
                                                )

main :: IO ()
main = do
  inputMap <- loadInput "input3.txt"
  let treeCountForSlope' = treeCountForSlope inputMap

  let firstSlope         = (3, 1)
  print $ treeCountForSlope' firstSlope

  let slopesToCheck = [(1, 1), firstSlope, (5, 1), (7, 1), (1, 2)]
  print $ product $ map treeCountForSlope' slopesToCheck
