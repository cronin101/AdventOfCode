{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    monkeyChain,
    monkeyBusiness,
    Monkey (getsBored),
  )
where

import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight, isRight)
import Data.IntMap qualified as IM
import Data.List (partition, sortBy)
import Data.Maybe (fromMaybe)
import GHC.Base ((<|>))

data Monkey = Monkey
  { index :: Int,
    items :: [Int],
    inspectedCount :: Int,
    operation :: Int -> Int,
    divisor :: Int,
    test :: Int -> Bool,
    recipients :: (Int, Int),
    getsBored :: Bool
  }

instance Show Monkey where
  show (Monkey idx is _ _ _ _ _ _) = show (idx, is) ++ "\n"

parseLiteralOperation :: A.Parser (Int -> Int)
parseLiteralOperation = do
  operator <- A.anyChar <* " "
  value <- A.decimal
  return $ case operator of
    '+' -> (+ value)
    '-' -> subtract value
    '*' -> (* value)
    '/' -> (`div` value)
    _ -> error "Invalid operator"

parseReferentialOperation :: A.Parser (Int -> Int)
parseReferentialOperation = do
  operator <- A.anyChar <* " old"
  return $ case operator of
    '+' -> join (+)
    '-' -> join (-)
    '*' -> join (*)
    '/' -> join div
    _ -> error "Invalid operator"

-- >>> isRight $ A.parseOnly parseOperation "Operation: new = old * 19"
-- True
-- >>> isRight $ A.parseOnly parseOperation "Operation: new = old * old"
-- True
parseOperation :: A.Parser (Int -> Int)
parseOperation = "Operation: new = old " *> (parseLiteralOperation <|> parseReferentialOperation)

-- >>> A.parseOnly parseMonkeyIndex "Monkey 0:"
-- Right 0
parseMonkeyIndex :: A.Parser Int
parseMonkeyIndex = "Monkey " *> A.decimal <* ":"

-- >>> A.parseOnly parseStartingItems "Starting items: 79, 98"
-- Right [79,98]
parseStartingItems :: A.Parser [Int]
parseStartingItems = "Starting items: " *> A.sepBy1 A.decimal ", "

-- >>> isRight $  A.parseOnly parseTest "Test: divisible by 23"
-- True
parseTest :: A.Parser (Int, Int -> Bool)
parseTest = do
  testValue <- "Test: divisible by " *> A.decimal
  return (testValue, (== 0) . (`mod` testValue))

-- >>> A.parseOnly parseRecipents " If true: throw to monkey 1\n   If false: throw to monkey 3"
-- Right (1,3)
parseRecipents :: A.Parser (Int, Int)
parseRecipents = do
  trueRecipient <- A.skipSpace *> "If true: throw to monkey " *> A.decimal <* A.endOfLine
  falseRecipient <- A.skipSpace *> "If false: throw to monkey " *> A.decimal
  return (trueRecipient, falseRecipient)

parseMonkey :: A.Parser Monkey
parseMonkey = do
  idx <- parseMonkeyIndex <* A.endOfLine
  is <- A.skipSpace *> parseStartingItems <* A.endOfLine
  op <- A.skipSpace *> parseOperation <* A.endOfLine
  (d, t) <- A.skipSpace *> parseTest <* A.endOfLine
  r <- parseRecipents
  return $ Monkey idx is 0 op d t r True

parseMonkeys :: A.Parser [Monkey]
parseMonkeys = A.sepBy1 parseMonkey (A.count 2 A.endOfLine)

loadInput :: String -> IO [Monkey]
loadInput = (fromRight [] . A.parseOnly parseMonkeys <$>) . BSC.readFile . ("src/" ++)

processMonkey :: Monkey -> (Monkey, IM.IntMap [Int])
processMonkey = toMap . partitionItems . transformWorry
  where
    toMap (monkey, (trueItems, falseItems)) = (monkey, IM.fromList [(trueRecipient, trueItems), (falseRecipient, falseItems)])
      where
        (trueRecipient, falseRecipient) = recipients monkey
    partitionItems monkey@(Monkey _ is iCount _ _ t _ _) = (monkey {items = [], inspectedCount = iCount + length is}, partition t is)
    transformWorry monkey@(Monkey _ is _ op _ _ _ getsBored) = monkey {items = map ((`div` if getsBored then 3 else 1) . op) is}

processMonkeys :: [Monkey] -> [Monkey]
processMonkeys = reduceItems . reverse . processMonkeys' IM.empty []
  where
    reduceItems :: [Monkey] -> [Monkey]
    reduceItems monkeys =
      let m = product (map divisor monkeys)
       in map (\monkey -> monkey {items = map (`mod` m) (items monkey)}) monkeys
    giveIncomingMonkeys incoming monkey = monkey {items = items monkey ++ fromMaybe [] (IM.lookup (index monkey) incoming)}
    processMonkeys' incomingMonkeys processedMonkeys [] = map (giveIncomingMonkeys incomingMonkeys) processedMonkeys
    processMonkeys' incomingMonkeys processedMonkeys (m : ms) =
      let (m', incoming) = processMonkey (giveIncomingMonkeys incomingMonkeys m)
       in processMonkeys' incoming (m' : map (giveIncomingMonkeys incomingMonkeys) processedMonkeys) (map (giveIncomingMonkeys incomingMonkeys) ms)

monkeyChain :: Int -> [Monkey] -> [Monkey]
monkeyChain n = (!! max 0 n) . iterate processMonkeys

inspectionChainLengths :: [Monkey] -> IM.IntMap Int
inspectionChainLengths = IM.fromList . map (\monkey -> (index monkey, inspectedCount monkey))

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare) . IM.elems . inspectionChainLengths
