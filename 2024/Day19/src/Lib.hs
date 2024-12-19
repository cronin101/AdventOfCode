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

data Input where
  Input :: {availableTowels :: [Towel], designs :: [BSC.ByteString]} -> Input
  deriving (Eq, Ord, Show)

data Colour = White | Blue | Black | Red | Green deriving (Eq, Ord)

instance Read Colour where
  readsPrec _ ('w' : rest) = [(White, rest)]
  readsPrec _ ('u' : rest) = [(Blue, rest)]
  readsPrec _ ('b' : rest) = [(Black, rest)]
  readsPrec _ ('r' : rest) = [(Red, rest)]
  readsPrec _ ('g' : rest) = [(Green, rest)]
  readsPrec _ _ = []

instance Show Colour where
  show White = "w"
  show Blue = "u"
  show Black = "b"
  show Red = "r"
  show Green = "g"

data Towel where
  Towel :: {colours' :: [Colour]} -> Towel
  deriving (Eq, Ord, Show)

parseColour :: A.Parser Colour
parseColour = read . pure <$> A.satisfy (A.inClass "wubrg")

-- >>> A.parseOnly parseTowel "bwu"
-- Right (Towel {colours' = [b,w,u]})
parseTowel :: A.Parser Towel
parseTowel = Towel <$> A.many1 parseColour

-- >>> A.parseOnly parseAvailableTowels "r, wr, b, g, bwu, rb, gb, br"
-- Right [Towel {colours' = [r]},Towel {colours' = [w,r]},Towel {colours' = [b]},Towel {colours' = [g]},Towel {colours' = [b,w,u]},Towel {colours' = [r,b]},Towel {colours' = [g,b]},Towel {colours' = [b,r]}]
parseAvailableTowels :: A.Parser [Towel]
parseAvailableTowels = parseTowel `A.sepBy` ", "

-- >>> A.parseOnly parseDesign "brwrr"
-- Right "brwrr"
parseDesign :: A.Parser BSC.ByteString
parseDesign = A.takeWhile1 (A.inClass "wubrg")

parseInput :: A.Parser Input
parseInput = Input <$> (parseAvailableTowels <* A.count 2 A.endOfLine) <*> (parseDesign `A.sepBy` A.endOfLine)

-- >>> loadInput "example.txt"
-- Input {availableTowels = [Towel {colours' = [Red]},Towel {colours' = [White,Red]},Towel {colours' = [Black]},Towel {colours' = [Green]},Towel {colours' = [Black,White,Blue]},Towel {colours' = [Red,Black]},Towel {colours' = [Green,Black]},Towel {colours' = [Black,Red]}], designs = ["brwrr","bggr","gbbr","rrbgbr","ubwu","bwurrg","brgr","bbrgwb"]}
loadInput :: [Char] -> IO Input
loadInput = (fromRight (Input [] []) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

-- >>> gobble "br" "brwrr"
-- Just "wrr"
gobble :: BSC.ByteString -> BSC.ByteString -> Maybe BSC.ByteString
gobble prefix bs =
  if prefix `BSC.isPrefixOf` bs
    then Just (BSC.drop (BSC.length prefix) bs)
    else Nothing

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

-- >>> part1 <$> loadInput "example.txt"
-- 6
part1 :: Input -> Int
part1 (Input ts ds) = length $ filter ((> 0) . possibleWays ts) ds

-- >>> part2 <$> loadInput "example.txt"
-- 16
part2 :: Input -> Int
part2 (Input ts ds) = sum $ map (possibleWays ts) ds
