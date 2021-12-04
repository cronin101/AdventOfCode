{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type BingoCard = [[Int]]

-- >>> A.parseOnly parseDigitsWithLeadingSpace " 23"
-- Right 23
parseDigitsWithLeadingSpace :: A.Parser Int
parseDigitsWithLeadingSpace = do
  A.skipSpace
  A.decimal

-- >>> A.parseOnly parseBingoCardRow "1 2 3 4"
-- Right [1,2,3,4]
parseBingoCardRow :: A.Parser [Int]
parseBingoCardRow = A.sepBy1 parseDigitsWithLeadingSpace $ A.many1 $ A.string " "

-- >>> A.parseOnly parseBingoCard "1 2\n3 4"
-- Right [[1,2],[3,4]]
parseBingoCard :: A.Parser BingoCard
parseBingoCard = A.sepBy1 parseBingoCardRow A.endOfLine

-- >>> A.parseOnly parseInput "1,2,3,4\n\n10 20\n30 40\n\n50 60\n70 81"
-- Right ([1,2,3,4],[[[10,20],[30,40]],[[50,60],[70,81]]])
parseInput :: A.Parser ([Int], [BingoCard])
parseInput = do
  calledNumbers <- A.sepBy1 A.decimal $ A.string ","
  A.count 2 A.endOfLine
  bingoCards <- A.sepBy1 parseBingoCard $ A.count 2 A.endOfLine
  return (calledNumbers, bingoCards)

-- >>> loadInput "example.txt"
-- Right ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],[[[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19],[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6],[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]])
loadInput fileName =
  fromRight ([], [])
    . A.parseOnly parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)
