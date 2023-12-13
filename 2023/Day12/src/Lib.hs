{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Map qualified as M

data Spring = Operational | Damaged | Unknown deriving (Show, Eq, Ord)

type SpringRow = ([Spring], [(Int, Int)])

-- >>> A.parseOnly parseVariableSegments ".#.###.#.######"
-- Right [Operational,Damaged,Operational,Damaged,Damaged,Damaged,Operational,Damaged,Operational,Damaged,Damaged,Damaged,Damaged,Damaged,Damaged]
parseVariableSegments :: A.Parser [Spring]
parseVariableSegments = A.many1 $ A.choice ["." $> Operational, "#" $> Damaged, "?" $> Unknown]

-- >>> A.parseOnly parseFixedSegments "1,1,3"
-- Right [(1,1),(1,1),(3,3)]
parseFixedSegments :: A.Parser [(Int, Int)]
parseFixedSegments = map (\x -> (x, x)) <$> A.decimal `A.sepBy1` ","

--- >>> A.parseOnly parseSpringRow "?#?#?#?#?#?#?#? 1,3,1,6"
-- Right ([Unknown,Damaged,Unknown,Damaged,Unknown,Damaged,Unknown,Damaged,Unknown,Damaged,Unknown,Damaged,Unknown,Damaged,Unknown],[(1,1),(3,3),(1,1),(6,6)])
parseSpringRow :: A.Parser SpringRow
parseSpringRow = do
  vs <- parseVariableSegments <* " "
  (vs,) <$> parseFixedSegments

impossible :: SpringRow -> Bool
impossible (vs, (x, _) : fs) = length vs < remaining || length (filter (== Damaged) vs) > remaining || length (filter (/= Operational) vs) < remaining || length (filter (/= Damaged) vs) < length fs
  where
    remaining = x + sum (map snd fs)

validCount :: SpringRow -> Int
validCount k = validCountWithCache M.empty k M.! k

validCountWithCache :: M.Map SpringRow Int -> SpringRow -> M.Map SpringRow Int
-- Finished variable segments and no more open fixedSegments
validCountWithCache c k@([], [(0, _)]) = M.insert k 1 c
-- Fail if there's no more variable segments but we still have an open fixed segment
validCountWithCache c k@([], _) = M.insert k 0 c
-- Finished fixed segments and only (potentially) Operational variable segments remain
validCountWithCache c k@(vs@(Operational : _), [(0, _)])
  | Damaged `notElem` vs = M.insert k 1 c
  | otherwise = M.insert k 0 c
-- Fail if we see a Damaged spring but we have no more space in our fixed segment
validCountWithCache c k@(Damaged : _, (0, _) : _) = M.insert k 0 c
-- ... otherwise Fail if we ran out of fixed segments and have something Damaged on the head
validCountWithCache c k@(_, []) = M.insert k 0 c
-- If we see an operational spring...
validCountWithCache c k@(vs@(Operational : _), fs@((left, total) : _))
  | M.member k c = c
  | impossible k = M.insert k 0 c
  -- Consume all if we haven't started a fixed segment...
  | left == total =
      let subKey = (dropWhile (== Operational) vs, fs)
          subProblem = validCountWithCache c subKey
       in M.insert k (subProblem M.! subKey) c `M.union` subProblem
  -- ... or if we just finished a fixed segment.
  | left == 0 =
      let subKey = (dropWhile (== Operational) vs, tail fs)
          subProblem = validCountWithCache c subKey
       in M.insert k (subProblem M.! subKey) c `M.union` subProblem
  -- Fail if we are mid-segment
  | otherwise = M.insert k 0 c
-- If we see a damaged spring...
validCountWithCache c k@(vs@(Damaged : _), (left, total) : fs)
  | M.member k c = c
  | impossible k = M.insert k 0 c
  -- Consume it and following damaged (and wildcards as damaged) while our segment has capacity left...
  | otherwise =
      let damagedCount = min left (length $ takeWhile (/= Operational) vs)
          vs' = drop damagedCount vs
          subKey = (vs', (left - damagedCount, total) : fs)
          subProblem = validCountWithCache c subKey
       in if damagedCount == left
            then M.insert k (subProblem M.! subKey) c `M.union` subProblem
            else --- ... otherwise fail
              M.insert k 0 c
-- When we see a wildcard, traverse both paths
validCountWithCache c k@(Unknown : vs, fs)
  | M.member k c = c
  | impossible k = M.insert k 0 c
  | otherwise =
      let leftKey = (Damaged : vs, fs)
          leftProblem = validCountWithCache c leftKey
          rightKey = (Operational : vs, fs)
          rightProblem = validCountWithCache (c `M.union` leftProblem) rightKey
       in M.insert k (leftProblem M.! leftKey + rightProblem M.! rightKey) c `M.union` rightProblem

-- >>> loadInput "example.txt"
-- [Spring {variableSegments = [(Nothing,3),(Just Broken,1),(Just Operational,3)], fixedSegments = [(Just Operational,1),(Just Operational,1),(Just Operational,3)]},Spring {variableSegments = [(Just Broken,1),(Nothing,2),(Just Broken,2),(Nothing,2),(Just Broken,3),(Nothing,1),(Just Operational,2),(Just Broken,1)], fixedSegments = [(Just Operational,1),(Just Operational,1),(Just Operational,3)]},Spring {variableSegments = [(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1),(Just Operational,1),(Nothing,1)], fixedSegments = [(Just Operational,1),(Just Operational,3),(Just Operational,1),(Just Operational,6)]},Spring {variableSegments = [(Nothing,4),(Just Broken,1),(Just Operational,1),(Just Broken,3),(Just Operational,1),(Just Broken,3)], fixedSegments = [(Just Operational,4),(Just Operational,1),(Just Operational,1)]},Spring {variableSegments = [(Nothing,4),(Just Broken,1),(Just Operational,6),(Just Broken,2),(Just Operational,5),(Just Broken,1)], fixedSegments = [(Just Operational,1),(Just Operational,6),(Just Operational,5)]},Spring {variableSegments = [(Nothing,1),(Just Operational,3),(Nothing,8)], fixedSegments = [(Just Operational,3),(Just Operational,2),(Just Operational,1)]}]
loadInput :: [Char] -> IO [SpringRow]
loadInput = (fromRight [] . A.parseOnly (parseSpringRow `A.sepBy1` A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 21
part1 :: [SpringRow] -> Int
part1 = sum . map validCount

-- >>> part2 <$> loadInput "example.txt"
-- 525152
part2 :: [SpringRow] -> Int
part2 = sum . map (validCount . expand)

expand :: SpringRow -> SpringRow
expand (vs, fs) = (intercalate [Unknown] (replicate 5 vs), concat $ replicate 5 fs)
