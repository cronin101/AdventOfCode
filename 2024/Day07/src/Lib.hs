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

type Operator = Int -> Int -> Int

-- Parses a single equation from the input.
-- Each equation consists of a target value and a list of numbers to be combined with operators.
-- >>> A.parseOnly parseEquation "1: 2 3 4"
-- Right (1,[2,3,4])
parseEquation :: A.Parser Equation
parseEquation = (,) <$> A.decimal <* ": " <*> (A.decimal `A.sepBy` " ")

-- Parses multiple equations from the input file.
parseEquations :: A.Parser [Equation]
parseEquations = parseEquation `A.sepBy` A.endOfLine

-- Determines all possible results from combining the numbers with the given operators.
-- >>> possibleResults [(+), (*), (||)] [10, 19]
-- fromList [29,190,1019]
possibleResults :: [Operator] -> [Int] -> IS.IntSet
possibleResults _ [] = IS.empty
possibleResults ops (x : xs) = possibleResults' (IS.singleton x) xs
  where
    possibleResults' acc [] = acc
    possibleResults' acc (y : ys) =
      let acc' = IS.unions $ map (\a -> IS.fromList (map (\f -> f a y) ops)) $ IS.toList acc
       in possibleResults' acc' ys

-- Loads the calibration equations from a file.
-- >>> loadInput "example.txt"
-- [(190,[10,19]),(3267,[81,40,27]),(83,[17,5]),(156,[15,6]),(7290,[6,8,6,15]),(161011,[16,10,13]),(192,[17,8,14]),(21037,[9,7,18,13]),(292,[11,6,16,20])]
loadInput :: [Char] -> IO [Equation]
loadInput = (fromRight [] . A.parseOnly parseEquations <$>) . BSC.readFile . ("src/" ++)

-- Checks if a given equation is valid by seeing if the target value can be produced by the operators.
isValid :: [Operator] -> Equation -> Bool
isValid ops (target, values) = IS.member target $ possibleResults ops values

-- Concatenates two numbers.
(||) :: Operator
(||) x y =
  let yDigits = length $ show y
   in x * 10 ^ yDigits + y

-- Finds the total calibration result for equations that can be made true using addition and multiplication.
-- >>> part1 <$> loadInput "example.txt"
-- 3749
part1 :: [Equation] -> Int
part1 = sum . map fst . filter (isValid [(*), (+)])

-- Finds the total calibration result for equations that can be made true using addition, multiplication, and concatenation.
-- >>> part2 <$> loadInput "example.txt"
-- 11387
part2 :: [Equation] -> Int
part2 = sum . map fst . filter (isValid [(*), (+), (||)])
