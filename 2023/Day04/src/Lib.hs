{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

data ScratchCard = ScratchCard
  { cardId :: Int,
    winningNumbers :: S.Set Int,
    cardNumbers :: S.Set Int,
    matchCount :: Int,
    cardScore :: Int
  }
  deriving (Show)

-- >>> A.parseOnly parseNumberList "41 48 83 86 17"
-- Right (fromList [17,41,48,83,86])
parseNumberList :: A.Parser (S.Set Int)
parseNumberList = S.fromList <$> A.decimal `A.sepBy` A.many1 A.space

-- >>> A.parseOnly parseScratchCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- Right (ScratchCard {cardId = 1, winningNumbers = fromList [17,41,48,83,86], cardNumbers = fromList [6,9,17,31,48,53,83,86], matchCount = 4, cardScore = 8})
parseScratchCard :: A.Parser ScratchCard
parseScratchCard = do
  cId <- "Card" *> A.many1 A.space *> A.decimal <* ":" <* A.many1 A.space
  (winning, revealed) <- (,) <$> parseNumberList <* (A.many1 A.space *> "|" <* A.many1 A.space) <*> parseNumberList
  let matchSize = S.size $ S.intersection winning revealed
  let score = if matchSize > 0 then 2 ^ (matchSize - 1) else 0
  return $ ScratchCard cId winning revealed matchSize score

-- >>> loadInput "example.txt"
-- [ScratchCard {cardId = 1, winningNumbers = fromList [17,41,48,83,86], cardNumbers = fromList [6,9,17,31,48,53,83,86], matchCount = 4, cardScore = 8},ScratchCard {cardId = 2, winningNumbers = fromList [13,16,20,32,61], cardNumbers = fromList [17,19,24,30,32,61,68,82], matchCount = 2, cardScore = 2},ScratchCard {cardId = 3, winningNumbers = fromList [1,21,44,53,59], cardNumbers = fromList [1,14,16,21,63,69,72,82], matchCount = 2, cardScore = 2},ScratchCard {cardId = 4, winningNumbers = fromList [41,69,73,84,92], cardNumbers = fromList [5,51,54,58,59,76,83,84], matchCount = 1, cardScore = 1},ScratchCard {cardId = 5, winningNumbers = fromList [26,28,32,83,87], cardNumbers = fromList [12,22,30,36,70,82,88,93], matchCount = 0, cardScore = 0},ScratchCard {cardId = 6, winningNumbers = fromList [13,18,31,56,72], cardNumbers = fromList [10,11,23,35,36,67,74,77], matchCount = 0, cardScore = 0}]
loadInput :: [Char] -> IO [ScratchCard]
loadInput = (fromRight [] . A.parseOnly (parseScratchCard `A.sepBy1` A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 13
part1 :: [ScratchCard] -> Int
part1 = sum . map cardScore

-- >>> part2 <$> loadInput "example.txt"
-- 30
part2 :: [ScratchCard] -> Int
part2 cards = sum $ IM.elems cardinalityMap
  where
    -- Dynamic programming!: Traverse !!backwards (thanks FoldingFromTheRight)!! to calculate the cardinality of a card from the summed cardinalities of cards that can be won from it (which have higher ID)
    cardinalityMap = foldr (\card partialMap -> IM.insert (cardId card) (1 + sum (mapMaybe ((`IM.lookup` partialMap) . cardId) (expandCard card))) partialMap) IM.empty cards
    -- Maps a card into the cards that are won
    expandCard card = mapMaybe (`IM.lookup` cardsById) $ take (matchCount card) [(cardId card + 1) ..]
    -- Efficient lookup of cards
    cardsById = IM.fromList $ map (\card -> (cardId card, card)) cards
