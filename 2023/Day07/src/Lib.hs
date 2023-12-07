{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

data HandType = HighCard | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Rank = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

type Hand = [Rank]

data ScoredHand = ScoredHand {handType :: HandType, hand :: Hand} deriving (Show, Eq, Ord)

type HandWithBid = (Hand, Int)

countedCards :: [Rank] -> M.Map Rank Int
countedCards c = M.fromListWith (+) [(x, 1) | x <- c]

score :: Hand -> ScoredHand
score c
  | 5 `S.member` counts = ScoredHand FiveOfAKind c
  | 4 `S.member` counts = ScoredHand FourOfAKind c
  | 3 `S.member` counts && 2 `S.member` counts = ScoredHand FullHouse c
  | 3 `S.member` counts = ScoredHand ThreeOfAKind c
  | 2 `S.member` counts && distinctRanks == length c - 2 = ScoredHand TwoPair c
  | 2 `S.member` counts = ScoredHand Pair c
  | otherwise = ScoredHand HighCard c
  where
    distinctRanks = S.size $ S.fromList c
    counts = S.fromList $ M.elems $ countedCards c

scoreWithJokers :: Hand -> ScoredHand
scoreWithJokers = recalculateScore . score . map (\case Jack -> Joker; x -> x)
  where
    recalculateScore (ScoredHand _ h) = ScoredHand (iterate applyJoker (handTypeWithoutJokers h) !! jokerCount h) h
    jokerCount h = fromMaybe 0 (M.lookup Joker (countedCards h))
    handTypeWithoutJokers = handType . score . filter (\case Joker -> False; _ -> True)
    -- This part was a bit hairy and needed some thought, I wanted to `succ $enum` but alas...
    applyJoker FiveOfAKind = FiveOfAKind
    applyJoker ThreeOfAKind = FourOfAKind
    applyJoker Pair = ThreeOfAKind
    applyJoker TwoPair = FullHouse
    applyJoker HighCard = Pair
    applyJoker FourOfAKind = FiveOfAKind
    applyJoker FullHouse = FiveOfAKind

parseRank :: A.Parser Rank
parseRank =
  A.choice
    [ "2" $> Two,
      "3" $> Three,
      "4" $> Four,
      "5" $> Five,
      "6" $> Six,
      "7" $> Seven,
      "8" $> Eight,
      "9" $> Nine,
      "T" $> Ten,
      "Q" $> Queen,
      "J" $> Jack,
      "K" $> King,
      "A" $> Ace
    ]

-- >>> A.parseOnly parseHandWithBid "32T3K 765"
-- Right ([Three,Two,Ten,Three,King],765)
parseHandWithBid :: A.Parser HandWithBid
parseHandWithBid = (,) <$> A.count 5 parseRank <*> (A.skipSpace *> A.decimal)

-- >>> loadInput "example.txt"
-- [([Three,Two,Ten,Three,King],765),([Ten,Five,Five,Jack,Five],684),([King,King,Six,Seven,Seven],28),([King,Ten,Jack,Jack,Ten],220),([Queen,Queen,Queen,Jack,Ace],483)]
loadInput :: [Char] -> IO [HandWithBid]
loadInput = (fromRight [] . A.parseOnly (parseHandWithBid `A.sepBy1` A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

winningsForHands :: (Hand -> ScoredHand) -> [HandWithBid] -> Int
winningsForHands scoringFunction hwbs = sum $ zipWith (\(_, bid) rank -> bid * rank) (sortHands hwbs) [1 ..]
  where
    sortHands = sortBy (compare `on` scoringFunction . fst)

-- >>> part1 <$> loadInput "example.txt"
-- 6440
part1 :: [HandWithBid] -> Int
part1 = winningsForHands score

-- >>> part2 <$> loadInput "example.txt"
-- 5905
part2 :: [HandWithBid] -> Int
part2 = winningsForHands scoreWithJokers
