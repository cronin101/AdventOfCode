{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

-- The Input data type represents the collection of available towels and the desired designs.
data Input where
  Input :: {availableTowels :: [Towel], designs :: [BSC.ByteString]} -> Input
  deriving (Eq, Ord, Show)

-- The Colour data type represents the possible colors of the stripes on the towels.
data Colour = White | Blue | Black | Red | Green deriving (Eq, Ord)

-- The Read instance for Colour allows us to parse single-character representations of colors.
instance Read Colour where
  readsPrec _ ('w' : rest) = [(White, rest)]
  readsPrec _ ('u' : rest) = [(Blue, rest)]
  readsPrec _ ('b' : rest) = [(Black, rest)]
  readsPrec _ ('r' : rest) = [(Red, rest)]
  readsPrec _ ('g' : rest) = [(Green, rest)]
  readsPrec _ _ = []

-- The Show instance for Colour allows us to convert colors back to their single-character representations.
instance Show Colour where
  show White = "w"
  show Blue = "u"
  show Black = "b"
  show Red = "r"
  show Green = "g"

-- The Towel data type represents a towel with a specific pattern of colors.
data Towel where
  Towel :: {colours' :: [Colour]} -> Towel
  deriving (Eq, Ord, Show)

-- The parseColour function parses a single character into a Colour.
parseColour :: A.Parser Colour
parseColour = read . pure <$> A.satisfy (A.inClass "wubrg")

-- The parseTowel function parses a sequence of characters into a Towel.
-- >>> A.parseOnly parseTowel "bwu"
-- Right (Towel {colours' = [b,w,u]})
parseTowel :: A.Parser Towel
parseTowel = Towel <$> A.many1 parseColour

-- The parseAvailableTowels function parses a list of towel patterns separated by ", ".
-- >>> A.parseOnly parseAvailableTowels "r, wr, b, g, bwu, rb, gb, br"
-- Right [Towel {colours' = [r]},Towel {colours' = [w,r]},Towel {colours' = [b]},Towel {colours' = [g]},Towel {colours' = [b,w,u]},Towel {colours' = [r,b]},Towel {colours' = [g,b]},Towel {colours' = [b,r]}]
parseAvailableTowels :: A.Parser [Towel]
parseAvailableTowels = parseTowel `A.sepBy` ", "

-- The parseDesign function parses a single design pattern.
-- >>> A.parseOnly parseDesign "brwrr"
-- Right "brwrr"
parseDesign :: A.Parser BSC.ByteString
parseDesign = A.takeWhile1 (A.inClass "wubrg")

-- The parseInput function parses the entire input, including available towels and designs.
parseInput :: A.Parser Input
parseInput = Input <$> (parseAvailableTowels <* A.count 2 A.endOfLine) <*> (parseDesign `A.sepBy` A.endOfLine)

-- The loadInput function reads the input from a file and parses it into the Input data type.
-- >>> loadInput "example.txt"
-- Input {availableTowels = [Towel {colours' = [Red]},Towel {colours' = [White,Red]},Towel {colours' = [Black]},Towel {colours' = [Green]},Towel {colours' = [Black,White,Blue]},Towel {colours' = [Red,Black]},Towel {colours' = [Green,Black]},Towel {colours' = [Black,Red]}], designs = ["brwrr","bggr","gbbr","rrbgbr","ubwu","bwurrg","brgr","bbrgwb"]}
loadInput :: [Char] -> IO Input
loadInput = (fromRight (Input [] []) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

-- The gobble function checks if a prefix matches the beginning of a ByteString and returns the remainder.
-- >>> gobble "br" "brwrr"
-- Just "wrr"
gobble :: BSC.ByteString -> BSC.ByteString -> Maybe BSC.ByteString
gobble prefix bs =
  if prefix `BSC.isPrefixOf` bs
    then Just (BSC.drop (BSC.length prefix) bs)
    else Nothing

-- The possibleWays function calculates the number of ways to create a design using the available towels.
-- >>> possibleWays [Towel [Red]] "r"
-- 1
possibleWays :: [Towel] -> BSC.ByteString -> Int
possibleWays t s = evalState (countWays s) M.empty
  where
    towelPatterns = map (BSC.pack . concatMap show . colours') t
    countWays :: BSC.ByteString -> State (M.Map BSC.ByteString Int) Int
    countWays remainder
      | BSC.null remainder = return 1
      | otherwise = do
          memo <- get
          case M.lookup remainder memo of
            Just n -> return n
            Nothing -> do
              count <- sum <$> mapM countWays (mapMaybe (`gobble` remainder) towelPatterns)
              modify' (M.insert remainder count)
              return count

-- The part1 function calculates the number of designs that can be created using the available towels.
-- >>> part1 <$> loadInput "example.txt"
-- 6
part1 :: Input -> Int
part1 (Input ts ds) = length $ filter ((> 0) . possibleWays ts) ds

-- The part2 function calculates the total number of ways to create all the designs using the available towels.
-- >>> part2 <$> loadInput "example.txt"
-- 16
part2 :: Input -> Int
part2 (Input ts ds) = sum $ map (possibleWays ts) ds
