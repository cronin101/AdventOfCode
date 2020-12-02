module Main where

import           Lib                            ( loadInput
                                                , parseLine
                                                , isValid1
                                                , isValid2
                                                )

main :: IO ()
main = do
  input <- loadInput "input2.txt"
  let count1 = length . filter isValid1 $ map parseLine input
  print count1
  let count2 = length . filter isValid2 $ map parseLine input
  print count2
