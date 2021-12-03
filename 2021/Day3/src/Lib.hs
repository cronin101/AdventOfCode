module Lib
  ( loadInput,
    gamma,
    epsilon,
    readBinary,
    oxygenRating,
    scrubberRating,
  )
where

import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Maybe (mapMaybe)
import Numeric (readInt)

-- >>> readBinary "101"
-- 5
readBinary :: String -> Integer
readBinary = fst . head . readInt 2 isDigit (read . pure)

--- >>> gamma ["101", "011", "100"]
-- "101"
gamma :: (Eq b, Ord b) => [[b]] -> [b]
gamma = map (getFromGroupsByLengthComparison maximumBy) . groupedDigits

--- >>> epsilon ["101", "011", "100"]
-- "010"
epsilon :: (Eq b, Ord b) => [[b]] -> [b]
epsilon = map (getFromGroupsByLengthComparison minimumBy) . groupedDigits

getFromGroupsByLengthComparison :: Foldable t1 => ((t1 a1 -> t1 a1 -> Ordering) -> t2 -> [a2]) -> t2 -> a2
getFromGroupsByLengthComparison cmp grp = head $ cmp (compare `on` length) grp

-- >>> groupedDigits ["101", "011", "100"]
-- [["0","11"],["00","1"],["0","11"]]
groupedDigits :: Ord a => [[a]] -> [[[a]]]
groupedDigits input = map (group . sort) $ transpose input

oxygenRating :: [String] -> String
oxygenRating = filterCandidatesByDigitComparison maximumBy

scrubberRating :: [String] -> String
scrubberRating = filterCandidatesByDigitComparison minimumBy

filterCandidatesByDigitComparison :: (([a1] -> [a1] -> Ordering) -> [[Char]] -> [Char]) -> [[Char]] -> [Char]
filterCandidatesByDigitComparison = filterCandidatesByDigitComparison' 0
  where
    filterCandidatesByDigitComparison' _ _ [candidate] = candidate
    filterCandidatesByDigitComparison' iteration comparison candidates = filterCandidatesByDigitComparison' (iteration + 1) comparison candidates'
      where
        candidates' = filter matchesTarget candidates
        matchesTarget c = c !! iteration == target
        target = getFromGroupsByLengthComparison comparison $ group $ sort candidatesAtPosition
        candidatesAtPosition = map (!! iteration) candidates

loadInput :: [Char] -> IO [String]
loadInput fileName =
  map (takeWhile isDigit . BSC.unpack) . BSC.lines
    <$> BSC.readFile
      ("src/" ++ fileName)
