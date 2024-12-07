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
import Data.IntSet qualified as IS
import Prelude hiding ((||))

type Equation = (Int, [Int])

-- >>> A.parseOnly parseEquation "1: 2 3 4"
-- Right (1,[2,3,4])
parseEquation :: A.Parser Equation
parseEquation = (,) <$> A.decimal <* ": " <*> (A.decimal `A.sepBy` " ")

parseEquations :: A.Parser [Equation]
parseEquations = parseEquation `A.sepBy` A.endOfLine

-- >>> possibleResults [10, 19]
-- fromList [29,190]
possibleResults :: [Int -> Int -> Int] -> [Int] -> IS.IntSet
possibleResults _ [] = IS.empty
possibleResults ops (x : xs) = possibleResults' (IS.singleton x) xs
  where
    possibleResults' acc [] = acc
    possibleResults' acc (y : ys) =
      let acc' = IS.unions $ map (\a -> IS.fromList (map (\f -> f a y) ops)) $ IS.toList acc
       in possibleResults' acc' ys

-- >>> loadInput "example.txt"
-- [(190,[10,19]),(3267,[81,40,27]),(83,[17,5]),(156,[15,6]),(7290,[6,8,6,15]),(161011,[16,10,13]),(192,[17,8,14]),(21037,[9,7,18,13]),(292,[11,6,16,20])]
loadInput :: [Char] -> IO [Equation]
loadInput = (fromRight ([]) . A.parseOnly parseEquations <$>) . BSC.readFile . ("src/" ++)

isValid :: [Int -> Int -> Int] -> Equation -> Bool
isValid ops (target, values) = IS.member target $ possibleResults ops values

(||) :: Int -> Int -> Int
(||) x y = read (show x ++ show y)

-- >>> part1 <$> loadInput "example.txt"
-- 3749
part1 :: [Equation] -> Int
part1 = sum . map fst . filter (isValid [(*), (+)])

-- >>> part2 <$> loadInput "example.txt"
-- 11387
part2 :: [Equation] -> Int
part2 = sum . map fst . filter (isValid [(*), (+), (||)])
