{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( loadInput, duplicateItem, getBadges
    ) where

import           Control.Monad                    (join)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Bifunctor                   (Bifunctor (bimap))
import qualified Data.ByteString.Char8            as BSC
import           Data.Char                        (isAlpha, isLower, isUpper,
                                                   ord)
import           Data.Either                      (fromRight)
import qualified Data.IntSet                      as S
import           Data.List.Split                  (chunksOf)

type Rucksack = (S.IntSet, S.IntSet)

-- >>> map scoreChar ['a'..'z']
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]

-- >>> map scoreChar ['A'..'Z']
-- [27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52]

scoreChar :: Char -> Int
scoreChar char
    | isLower char = ord char - 96
    | isUpper char = ord char - 38
    | otherwise = 0

-- >>> A.parseOnly parseRucksack "vJrwpWtwJgWrhcsFMMfFFhFp"
-- Right (fromList [7,16,18,20,22,23,36,49],fromList [3,6,8,16,19,32,39])
parseRucksack :: A.Parser Rucksack
parseRucksack = do
    contents <- A.takeWhile isAlpha
    let splitPoint = BSC.length contents `div` 2
    return $ join bimap (S.fromList . map scoreChar . BSC.unpack) $ BSC.splitAt splitPoint contents

parseRucksacks :: A.Parser [Rucksack]
parseRucksacks = A.sepBy1 parseRucksack A.endOfLine

-- >>> duplicateItem $ fromRight (S.empty, S.empty) $  A.parseOnly parseRucksack "vJrwpWtwJgWrhcsFMMfFFhFp"
-- 16

duplicateItem :: Rucksack -> Int
duplicateItem (x, y) = S.findMin $ S.intersection x y

toBadgeGroups :: [Rucksack] -> [[Rucksack]]
toBadgeGroups = chunksOf 3

getBadges :: [Rucksack] -> [Int]
getBadges = map (S.findMin . foldl1 S.intersection . map (uncurry  S.union)) . toBadgeGroups

loadInput :: [Char] -> IO [Rucksack]
loadInput fileName =
  fromRight [] . A.parseOnly parseRucksacks <$> BSC.readFile ("src/" ++ fileName)
