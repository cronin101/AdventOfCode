{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , intervals
  , countOccurrences
  , includeTerminals
  , validArrangementsCount
  ) where

import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( foldl' )

includeTerminals :: IntMap Int -> IntMap Int
includeTerminals count = foldl' (\m v -> IM.insertWith (+) v 1 m) count [1, 3]

countOccurrences :: [Int] -> IntMap Int
countOccurrences = foldl' (\m v -> IM.insertWith (+) v 1 m) IM.empty

intervals :: IntSet -> [Int]
intervals adaptors = map (uncurry (-)) adaptorPairs
 where
  adaptorPairs      = zip (drop 1 ascendingAdaptors) ascendingAdaptors
  ascendingAdaptors = IS.toAscList adaptors

validArrangementsCount :: IntSet -> Int
validArrangementsCount adaptors = sum
  $ map (validArrangementsCount' [1, 0, 0]) [1 .. 3]
 where
  target = 3 + IS.findMax adaptors
  validArrangementsCount' frontier current
    | current == target = sum frontier
    | otherwise         = validArrangementsCount' (n' : frontier') next
   where
    frontier' = take 3 frontier
    n'        = if IS.member next adaptors then sum frontier' else 0
    next      = current + 1

loadInput :: String -> IO IntSet
loadInput fileName =
  IS.fromList . mapMaybe (fmap fst . BSC.readInt) . BSC.lines <$> BSC.readFile
    ("src/" ++ fileName)
