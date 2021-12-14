{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    stepN,
    quantityDelta,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.Map as M

-- (IfSeen, Insert)
type InsertionRule = (String, Char)

type InsertionRuleMap = M.Map String Char

-- (InitialPolymer, Rules)
type State = (String, InsertionRuleMap)

-- (Head, PairCount, Rules)
type State' = (Char, M.Map (Char, Char) Int, InsertionRuleMap)

-- >>> A.parseOnly parsePolymer $ "NNCB"
-- Right "NNCB"
parsePolymer :: A.Parser String
parsePolymer = BSC.unpack <$> A.takeWhile1 A.isAlpha_ascii

-- >>> A.parseOnly parseInsertionRule $ "CH -> B"
-- Right ("CH",'B')
parseInsertionRule :: A.Parser InsertionRule
parseInsertionRule = do
  ifSeen <- parsePolymer
  " -> "
  insert <- head <$> parsePolymer
  return (ifSeen, insert)

-- >>> A.parseOnly parseInsertionRules $ "CH -> B\nHH -> N"
-- Right (fromList [("CH",'B'),("HH",'N')])
parseInsertionRules :: A.Parser InsertionRuleMap
parseInsertionRules = M.fromList <$> A.sepBy1 parseInsertionRule A.endOfLine

parseInput :: A.Parser State
parseInput = do
  initialPolymer <- parsePolymer
  A.count 2 A.endOfLine
  rules <- parseInsertionRules
  return (initialPolymer, rules)

optimiseState :: State -> State'
optimiseState (polymer, rules) = (head polymer, M.unionsWith (+) $ zipWith (curry (`M.singleton` 1)) polymer (drop 1 polymer), rules)

-- >>> loadInput "example.txt"
-- ('N',fromList [(('C','B'),1),(('N','C'),1),(('N','N'),1)],fromList [("BB",'N'),("BC",'B'),("BH",'H'),("BN",'B'),("CB",'H'),("CC",'N'),("CH",'B'),("CN",'C'),("HB",'C'),("HC",'B'),("HH",'N'),("HN",'C'),("NB",'B'),("NC",'B'),("NH",'C'),("NN",'C')])
loadInput :: [Char] -> IO State'
loadInput fileName =
  optimiseState . fromRight ("", M.empty)
    . A.parseOnly parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> elementCounts  <$> loadInput "example.txt"
-- fromList [('B',1),('C',1),('N',2)]
elementCounts :: State' -> M.Map Char Int
elementCounts (head, pairsMap, _) = M.insertWith (+) head 1 $ M.unionsWith (+) $ map (uncurry (M.singleton . snd)) $ M.toList pairsMap

-- >>> step <$> loadInput "example.txt"
-- ('N',fromList [(('B','C'),1),(('C','H'),1),(('C','N'),1),(('H','B'),1),(('N','B'),1),(('N','C'),1)],fromList [("BB",'N'),("BC",'B'),("BH",'H'),("BN",'B'),("CB",'H'),("CC",'N'),("CH",'B'),("CN",'C'),("HB",'C'),("HC",'B'),("HH",'N'),("HN",'C'),("NB",'B'),("NC",'B'),("NH",'C'),("NN",'C')])
step :: State' -> State'
step (head, pairs, rules) = (head, nextPairs, rules)
  where
    nextPairs = M.unionsWith (+) $ map (M.fromList . expandPair) $ M.toList pairs
    expandPair pair@((left, right), count) = case M.lookup [left, right] rules of
      Just middle -> [((left, middle), count), ((middle, right), count)]
      _ -> pure pair

stepN :: Int -> State' -> State'
stepN = flip $ (!!) . iterate step

-- >>> quantityDelta . stepN 10 <$> loadInput "example.txt"
-- 1588
quantityDelta :: State' -> Int
quantityDelta state = maximum counts - minimum counts
  where
    counts = M.elems $ elementCounts state

-- >>> quantityDelta . stepN 40  <$> loadInput "example.txt"
-- 2188189693529
