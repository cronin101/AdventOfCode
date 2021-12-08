{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    countEasyDigitsInOutput,
    convertInput,
  )
where

import Control.Arrow (first)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (ord)
import Data.Either (fromRight)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (permutations)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type SegmentDigit = IS.IntSet

-- (InputDigits, OutputDigits)
type PuzzleInput = ([SegmentDigit], [SegmentDigit])

type PuzzleOutput = (Int, Int)

type WireMapping = IM.IntMap Int

-- >>> A.parseOnly parseSegmentDigit "cfbegad"
-- Right (fromList [0,1,2,3,4,5,6])
parseSegmentDigit :: A.Parser SegmentDigit
parseSegmentDigit = do
  letters <- A.takeWhile1 A.isAlpha_ascii
  return $ IS.fromList $ map ((\c -> c - ord 'a') . ord) $ BSC.unpack letters

parseSegments :: A.Parser [SegmentDigit]
parseSegments = A.sepBy1 parseSegmentDigit " "

parseInput :: A.Parser PuzzleInput
parseInput = do
  inputDigits <- parseSegments
  " | "
  outputDigits <- parseSegments
  return (inputDigits, outputDigits)

parseInputs :: A.Parser [PuzzleInput]
parseInputs = A.sepBy1 parseInput A.endOfLine

loadInput :: [Char] -> IO [PuzzleInput]
loadInput fileName =
  fromRight []
    . A.parseOnly parseInputs
    <$> BSC.readFile
      ("src/" ++ fileName)

countEasyDigitsInOutput :: PuzzleInput -> Int
countEasyDigitsInOutput (_, output) = length $ filter isEasySegment output
  where
    isEasySegment :: SegmentDigit -> Bool
    isEasySegment segment = case IS.size segment of
      2 -> True
      3 -> True
      4 -> True
      7 -> True
      _ -> False

segmentValues :: M.Map SegmentDigit Int
segmentValues = M.fromList $ map (first lettersToSegmentDigit) [("abcefg", 0), ("cf", 1), ("acdeg", 2), ("acdfg", 3), ("bcdf", 4), ("abdfg", 5), ("abdefg", 6), ("acf", 7), ("abcdefg", 8), ("abcdfg", 9)]
  where
    lettersToSegmentDigit :: String -> SegmentDigit
    lettersToSegmentDigit = IS.fromList . map ((\c -> c - ord 'a') . ord)

knownSegments :: S.Set SegmentDigit
knownSegments = M.keysSet segmentValues

allMappings :: [WireMapping]
allMappings = map (IM.fromList . zip [0 .. 6]) $ permutations [0 .. 6]

transformSegmentViaMapping :: IM.IntMap IS.Key -> IS.IntSet -> IS.IntSet
transformSegmentViaMapping mapping = IS.map (\wire -> fromJust $ IM.lookup wire mapping)

reducePossibleMappingsForSegment :: [IM.IntMap IS.Key] -> IS.IntSet -> [IM.IntMap IS.Key]
reducePossibleMappingsForSegment possibilities segment = filter (isPossibleMapping segment) possibilities
  where
    isPossibleMapping segment mapping = S.member (transformSegmentViaMapping mapping segment) knownSegments

givenExample :: ([SegmentDigit], [SegmentDigit])
givenExample = fromRight ([], []) $ A.parseOnly parseInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

givenSegments :: [SegmentDigit]
givenSegments = input ++ output
  where
    (input, output) = givenExample

-- >>> possibleMappingsForSegment givenSegments
-- fromList [(0,2),(1,5),(2,6),(3,0),(4,1),(5,3),(6,4)]
possibleMappingForSegments :: Foldable t => t IS.IntSet -> IM.IntMap IS.Key
possibleMappingForSegments = head . foldl reducePossibleMappingsForSegment allMappings

-- >>> convertInput givenExample
-- (8523796401,5353)
convertInput :: ([SegmentDigit], [SegmentDigit]) -> (Int, Int)
convertInput (input, output) = (toCode input, toCode output)
  where
    toCode :: [SegmentDigit] -> Int
    toCode = read . concatMap (show . getTransformedValue)
    getTransformedValue = fromJust . (`M.lookup` segmentValues) . transformSegmentViaMapping mapping
    mapping = possibleMappingForSegments (input ++ output)
