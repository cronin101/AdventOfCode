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
import Data.Char qualified as A
import Data.Either (fromRight)
import Data.Map qualified as M
import NumericPrelude (extendedGCD)

type INode a = (a, (a, a))

type Node = INode BSC.ByteString

type NodeMap = M.Map BSC.ByteString (BSC.ByteString, BSC.ByteString)

data Network = Network {route :: String, nodeMap :: NodeMap}
  deriving (Show)

-- >>> A.parseOnly parseNode "AAA = (BBB, CCC)"
-- Right ("AAA",("BBB","CCC"))
parseNode :: A.Parser Node
parseNode = (,) <$> (nodeName <* " = ") <*> ((,) <$> ("(" *> nodeName <* ", ") <*> nodeName <* ")")
  where
    nodeName = A.takeWhile1 A.isAlphaNum

parseNetwork :: A.Parser Network
parseNetwork = do
  route <- BSC.unpack <$> A.takeWhile1 A.isAlphaNum <* A.count 2 A.endOfLine
  nodes <- M.fromList <$> (parseNode `A.sepBy1` A.endOfLine) <* A.endOfInput
  pure $ Network route nodes

-- >>> loadInput "example.txt"
-- Network {route = "RL", nodeMap = fromList [("AAA",("BBB","CCC")),("BBB",("DDD","EEE")),("CCC",("ZZZ","GGG")),("DDD",("DDD","DDD")),("EEE",("EEE","EEE")),("GGG",("GGG","GGG")),("ZZZ",("ZZZ","ZZZ"))]}
loadInput :: [Char] -> IO Network
loadInput = (fromRight (Network [] M.empty) . A.parseOnly parseNetwork <$>) . BSC.readFile . ("src/" ++)

solveLength :: NodeMap -> BSC.ByteString -> (BSC.ByteString -> Bool) -> String -> Int -> Int
solveLength nodemap value predicate remainingRoute distance
  | predicate value = distance
  | otherwise =
      let value' = case head remainingRoute of { 'L' -> fst; _ -> snd } $ nodemap M.! value
       in solveLength nodemap value' predicate (tail remainingRoute) (distance + 1)

depthAndValueOfRepeat :: NodeMap -> Int -> String -> BSC.ByteString -> (Int, BSC.ByteString)
depthAndValueOfRepeat nodes stepCycleLength stepsAtStart start = step M.empty start stepsAtStart 0
  where
    step visited value steps depth =
      let position = (value, take stepCycleLength steps)
       in case M.lookup position visited of
            Just firstDepth -> (depth - firstDepth, value)
            Nothing -> step (M.insert position depth visited) ((case head steps of 'L' -> fst; _ -> snd) (nodes M.! value)) (tail steps) (depth + 1)

findCycleLength :: NodeMap -> Int -> String -> BSC.ByteString -> Int
findCycleLength nodes stepCycleLength stepsAtStart start =
  let (cycleOffset, cycleHead) = depthAndValueOfRepeat nodes stepCycleLength stepsAtStart start
   in fst $ depthAndValueOfRepeat nodes stepCycleLength (drop cycleOffset stepsAtStart) cycleHead

findPeriodAndOffset :: Network -> (BSC.ByteString -> Bool) -> BSC.ByteString -> (Int, Int)
findPeriodAndOffset network predicate start = (period, offset)
  where
    offset = valueDepth
    valueDepth = solveLength (nodeMap network) start predicate steps 0
    period = findCycleLength (nodeMap network) (length $ route network) steps start
    steps = cycle $ route network

findManyPeriodsAndOffsets :: (BSC.ByteString -> Bool) -> (BSC.ByteString -> Bool) -> Network -> [(Int, Int)]
findManyPeriodsAndOffsets startPredicate endPredicate network = map (findPeriodAndOffset network endPredicate) $ filter startPredicate $ M.keys (nodeMap network)

-- https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
combineCycles :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineCycles (periodA, offsetA) (periodB, offsetB) = (period, offset)
  where
    offset = (offsetA - (kA * delta * periodA)) `mod` period
    period = periodA * periodB `div` gcd
    delta = (offsetA - offsetB) `div` gcd
    (gcd, (kA, _)) = extendedGCD periodA periodB

-- >>> part1 <$> loadInput "example.txt"
-- 2
part1 :: Network -> Int
part1 (Network r nodes) = solveLength nodes "AAA" (== "ZZZ") (cycle r) 0

-- >>> part2 <$> loadInput "example3.txt"
-- 6
part2 :: Network -> Int
part2 = fst . foldl1 combineCycles . findManyPeriodsAndOffsets ((== 'A') . BSC.last) ((== 'Z') . BSC.last)
