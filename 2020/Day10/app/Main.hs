module Main where

import           Lib                            ( loadInput
                                                , intervals
                                                , countOccurrences
                                                , includeTerminals
                                                , validArrangementsCount
                                                )
import           Control.Monad                  ( liftM2 )
import qualified Data.IntMap.Strict            as M

main :: IO ()
main = do
  input <- loadInput "input10.txt"
  let intervalCounts = includeTerminals . countOccurrences $ intervals input
  print intervalCounts
  print $ liftM2 (*) (M.lookup 1 intervalCounts) (M.lookup 3 intervalCounts)
  print $ validArrangementsCount input
