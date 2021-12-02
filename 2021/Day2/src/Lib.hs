{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    sumDeltas,
    calculatePositionWithAim,
  )
where

import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    choice,
    endOfLine,
    inClass,
    isDigit,
    parseOnly,
    satisfy,
    sepBy1,
    space,
    string,
    takeWhile1,
  )
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Maybe (fromJust)

-- (x, y)
type PositionDelta = (Int, Int)

-- (x, y, AIM)
type PositionWithAim = (Int, Int, Int)

parseDigitsAsInt :: Parser Int
parseDigitsAsInt = fst . fromJust . BSC.readInt <$> takeWhile1 isDigit

parseForward :: Parser PositionDelta
parseForward = do
  string "forward"
  space
  distance <- parseDigitsAsInt
  return (distance, 0)

parseUp :: Parser PositionDelta
parseUp = do
  string "up"
  space
  distance <- parseDigitsAsInt
  return (0, - distance)

parseDown :: Parser PositionDelta
parseDown = do
  string "down"
  space
  distance <- parseDigitsAsInt
  return (0, distance)

parseDelta :: Parser PositionDelta
parseDelta = choice [parseForward, parseUp, parseDown]

parseDeltas :: Parser [PositionDelta]
parseDeltas = sepBy1 parseDelta endOfLine

sumDeltas :: [PositionDelta] -> PositionDelta
sumDeltas = join bimap sum . unzip

calculatePositionWithAim :: [PositionDelta] -> PositionDelta
calculatePositionWithAim deltas = (x, y)
  where
    (x, y, aim) = foldl' updatePositionWithAim (0, 0, 0) deltas
    updatePositionWithAim (x', y', aim) (0, y) = (x', y', aim + y)
    updatePositionWithAim (x', y', aim) (x, 0) = (x' + x, y' + (aim * x), aim)
    updatePositionWithAim rest _ = rest

loadInput :: String -> IO [PositionDelta]
loadInput fileName =
  fromRight [] . parseOnly parseDeltas
    <$> BSC.readFile
      ("src/" ++ fileName)