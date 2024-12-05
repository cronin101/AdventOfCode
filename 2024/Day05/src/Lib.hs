{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Maybe (mapMaybe)

type Rules = IM.IntMap IS.IntSet

data Input = Input Rules [[Int]]
  deriving (Show)

-- >>> A.parseOnly parseRule "47|53"
-- Right (47,fromList [53])
parseRule :: A.Parser (Int, IS.IntSet)
parseRule = (,) <$> A.decimal <*> (IS.singleton <$> ("|" *> A.decimal))

-- >>> A.parseOnly parseRules "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n"
-- Right (fromList [(29,fromList [13]),(47,fromList [13,29,53,61]),(53,fromList [13,29]),(61,fromList [13,29,53]),(75,fromList [13,29,47,53,61]),(97,fromList [13,29,47,53,61,75])])
parseRules :: A.Parser Rules
parseRules = IM.fromListWith IS.union <$> parseRule `A.sepBy1` A.endOfLine

-- >>> A.parseOnly parseUpdate "75,47,61,53,29"
-- Right [75,47,61,53,29]
parseUpdate :: A.Parser [Int]
parseUpdate = A.decimal `A.sepBy1` ","

parseUpdates :: A.Parser [[Int]]
parseUpdates = parseUpdate `A.sepBy1` A.endOfLine

parseInput :: A.Parser Input
parseInput = Input <$> parseRules <* A.count 2 A.endOfLine <*> parseUpdates

-- >>> loadInput "example.txt"
-- Input (fromList [(29,fromList [13]),(47,fromList [13,29,53,61]),(53,fromList [13,29]),(61,fromList [13,29,53]),(75,fromList [13,29,47,53,61]),(97,fromList [13,29,47,53,61,75])]) [[75,47,61,53,29],[97,61,53,29,13],[75,29,13],[75,97,47,61,53],[61,13,29],[97,13,75,29,47]]
loadInput :: [Char] -> IO Input
loadInput = (fromRight (Input IM.empty []) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

-- >>> compareUsing (IM.fromList [(29, IS.fromList [11])]) 29 11
-- LT
-- >>> compareUsing (IM.fromList [(29, IS.fromList [11])]) 11 29
-- GT
compareUsing :: Rules -> Int -> Int -> Ordering
compareUsing rules left right
  | left == right = EQ
  | otherwise =
      -- If the left element is in the right set, then the left element is greater than the right element.
      if maybe False (left `IS.member`) $ IM.lookup right rules
        then GT
        else LT

isValidAccordingTo :: Rules -> [Int] -> Bool
isValidAccordingTo rules update = all (== LT) . zipWith (compareUsing rules) update $ tail update

-- >>> sortUsing (IM.fromList [(29, IS.fromList [11])]) [11, 29]
-- [29,11]
sortUsing :: Rules -> [Int] -> [Int]
sortUsing rules update = solve initialRules initialFrontier []
  where
    -- Initial rules are filtered to include only those that intersect with the initial frontier.
    initialRules = IM.filter (not . IS.null . IS.intersection initialFrontier) rules
    initialFrontier = IS.fromList update
    solve rs f acc =
      if IS.null f
        then acc -- If the frontier is empty, return the accumulated result list.
        else
          ( let -- Find the next element to process, which is any element in the frontier that is not a key in the rules.
                next = IS.findMin $ f IS.\\ IM.keysSet rs
                f' = IS.delete next f
                -- Update the rules by removing the next element and filtering out those that no longer intersect with the updated frontier.
                rs' = IM.filter (not . IS.null . IS.intersection f') (IM.delete next rs)
                a' = next : acc
             in solve rs' f' a'
          )

-- >>> part1 <$> loadInput "example.txt"
-- 143
part1 :: Input -> Int
part1 (Input rules updates) = sum . mapMaybe middleElement . filter (isValidAccordingTo rules) $ updates

-- >>> part2 <$> loadInput "example.txt"
-- 123
part2 :: Input -> Int
part2 (Input rules updates) = sum . mapMaybe (middleElement . sortUsing rules) . filter (not . isValidAccordingTo rules) $ updates

-- >>> middleElement [1, 2, 3]
-- Just 2
middleElement :: [a] -> Maybe a
middleElement [] = Nothing
middleElement xs = Just (xs !! (length xs `div` 2))
