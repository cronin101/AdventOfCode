module Lib
  ( loadInput
  , calculateSeatId
  , missingId
  ) where

import qualified Data.IntSet                   as S
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BSC

-- Represents a position among a single dimension
data Position = Specific Int | Range (Int, Int)
    deriving (Show)

-- (x, y) location of seat
type SeatLocation = (Position, Position)

data Bisection = Low | High

maxRow :: Int
maxRow = 127
maxColumn :: Int
maxColumn = 7

possibleSeatLocation :: SeatLocation
possibleSeatLocation = (Range (0, maxColumn), Range (0, maxRow))

bisect :: Position -> Bisection -> Position
bisect p@(Specific _) _ = p
bisect (Range (low, high)) bisection
  | rangeLength == 2 = case bisection of
    Low  -> Specific low
    High -> Specific high
  | otherwise = case bisection of
    Low  -> Range (low, mid - 1)
    High -> Range (mid, high)
 where
  mid         = low + newLength
  newLength   = rangeLength `div` 2
  rangeLength = (high - low) + 1

locateSeat :: ByteString -> SeatLocation
locateSeat = BSC.foldl' updatePosition possibleSeatLocation
 where
  updatePosition (xPosition, yPosition) char = case char of
    'L' -> (bisect xPosition Low, yPosition)
    'R' -> (bisect xPosition High, yPosition)
    'F' -> (xPosition, bisect yPosition Low)
    'B' -> (xPosition, bisect yPosition High)

idForLocation :: SeatLocation -> Int
idForLocation (Specific x, Specific y) = (8 * y) + x

calculateSeatId :: ByteString -> Int
calculateSeatId = idForLocation . locateSeat

-- The missing seat id has left and right (-1 / +1) neighbours 
missingId :: [Int] -> Int
missingId ids = S.findMin $ S.intersection left right S.\\ idSet
 where
  idSet = S.fromList ids
  left  = S.map (+ (-1)) idSet
  right = S.map (+ 1) idSet

loadInput :: String -> IO [ByteString]
loadInput fileName = BSC.lines <$> BSC.readFile ("src/" ++ fileName)
