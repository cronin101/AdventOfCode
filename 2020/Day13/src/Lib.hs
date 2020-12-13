{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , nextDeparture
  , TimeConstraints(TimeConstraints)
  , combineCycles
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Control.Arrow                  ( Arrow(first) )
import           Math.NumberTheory.GCD          ( extendedGCD )

data TimeConstraints a = TimeConstraints
  { earliestDeparture :: a
  , busses            :: [(a, a)]
  }
  deriving Show

nextDeparture :: Int -> Int -> Int
nextDeparture earliest busId = previousDeparture + busId
  where previousDeparture = (earliest `quot` busId) * busId

parseBusses :: [BSC.ByteString] -> [(Int, Int)]
parseBusses busIds =
  map (first (fromIntegral . fromJust)) $ filter (isJust . fst) $ zip
    (map (fmap fst . BSC.readInt) $ busIds)
    [0 ..]

-- https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
combineCycles :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combineCycles (periodA, offsetA) (periodB, offsetB) = (period, offset)
 where
  offset       = (offsetA - (kA * delta * periodA)) `mod` period
  period       = periodA * periodB `div` gcd
  delta        = (offsetA - offsetB) `div` gcd
  (gcd, kA, _) = extendedGCD periodA periodB

loadInput :: String -> IO (TimeConstraints Int)
loadInput fileName = do
  raw <- BSC.readFile ("src/" ++ fileName)
  let [earliestDepartureRaw, busIdsRaw] = BSC.lines raw
  let Just (earliestDeparture, _)       = BSC.readInt earliestDepartureRaw
  let busses                            = parseBusses $ BSC.split ',' busIdsRaw
  return $ TimeConstraints (fromIntegral earliestDeparture) busses
