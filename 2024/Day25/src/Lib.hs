{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (partition, transpose)
import Data.Maybe (catMaybes)
import Data.Set qualified as S

data Mechanism where
  Lock :: [Int] -> Mechanism
  Key :: [Int] -> Mechanism
  deriving (Show, Eq, Ord)

parseMechanismBody :: Char -> A.Parser [[Maybe Int]]
parseMechanismBody c =
  (:)
    <$> (replicate 5 (if c == '#' then Just 1 else Nothing) <$ A.count 5 (A.char c))
    <* endOfLine
    <*> A.count 6 (A.count 5 ((Just 1 <$ "#") <|> (Nothing <$ ".")) <* A.option () endOfLine)

toHeights :: [Int] -> [[Maybe Int]] -> [Int]
toHeights r =
  map (maximum . catMaybes)
    . transpose
    . zipWith (map . fmap . (*)) r

-- >>> A.parseOnly parseLock "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n"
-- Right (Lock [0,5,3,4,3])
parseLock :: A.Parser Mechanism
parseLock = Lock . toHeights [0 ..] <$> parseMechanismBody '#'

-- >>> A.parseOnly parseKey ".....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n"
-- Right (Key [5,0,2,1,3])
parseKey :: A.Parser Mechanism
parseKey = Key . toHeights [6, 5 .. 0] <$> parseMechanismBody '.'

parseMechanisms :: A.Parser [Mechanism]
parseMechanisms = (parseLock <|> parseKey) `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- [Lock [0,5,3,4,3],Lock [1,2,0,5,3],Key [5,0,2,1,3],Key [4,3,4,0,2],Key [3,0,2,0,1]]
loadInput :: String -> IO [Mechanism]
loadInput = (fromRight [] . A.parseOnly parseMechanisms <$>) . BSC.readFile . ("src/" ++)

fit :: Mechanism -> Mechanism -> Bool
fit (Lock xs) (Key ys) = all (<= 5) $ zipWith (+) xs ys
fit (Key xs) (Lock ys) = fit (Lock ys) (Key xs)
fit _ _ = False

-- >>> part1 <$> loadInput "example.txt"
-- 3
part1 :: [Mechanism] -> Int
part1 ms =
  let (locks, keys) = partition (\case Lock _ -> True; _ -> False) ms
   in S.size $ S.fromList [(l, k) | l <- locks, k <- keys, fit l k]
