module Main where

import           Lib                            ( loadInput
                                                , nextDeparture
                                                , TimeConstraints
                                                  ( TimeConstraints
                                                  )
                                                , combineCycles
                                                )
import           Data.Ord                       ( comparing )
import           Data.List                      ( minimumBy )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Control.Arrow                  ( Arrow(second) )

main :: IO ()
main = do
  TimeConstraints theoreticalEarliestDeparture busses <- loadInput "input13.txt"
  let busIds = map fst busses
  let nextBusDepartures =
        zip busIds $ map (nextDeparture theoreticalEarliestDeparture) busIds
  let earliestDeparture = minimumBy (comparing snd) nextBusDepartures
  let (busId, delay) =
        second (subtract theoreticalEarliestDeparture) earliestDeparture
  print $ busId * delay

  -- Need to cast to Integer to prevent overflow
  print $ snd . foldl1 combineCycles $ map
    (bimap toInteger ((0 -) . toInteger))
    busses
