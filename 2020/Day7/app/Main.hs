module Main where

import           Lib                            ( loadInput
                                                , countContainerBags
                                                , countContainedBags
                                                , toDualRules
                                                )
import           Data.ByteString.Char8          ( pack )

main :: IO ()
main = do
  rules <- loadInput "input7.txt"
  let targetBag                      = pack "shiny gold"
  let (canContain, canBeContainedIn) = toDualRules rules
  print $ countContainerBags canBeContainedIn targetBag
  print $ countContainedBags canContain targetBag
