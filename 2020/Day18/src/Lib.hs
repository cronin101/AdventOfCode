{-#  LANGUAGE Strict #-}

module Lib
  ( loadInput
  , sumTotals
  , sumTotals2
  ) where

import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( find )

data Operation = Add | Mult
    deriving (Show)

type Instruction = (Operation, Scope)

data Scope = Literal Int | Scope [Instruction]
    deriving (Show)

merge :: Ord a => [a] -> [a] -> [a]
merge (x : xs) (y : ys) =
  if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys
merge [] xs = xs
merge xs [] = xs

parseInput :: Operation -> B.ByteString -> [Instruction]
parseInput op bs
  | B.null bs = []
  | otherwise = case B.readInt bs of
    Just (i, rest) -> (op, Literal i) : parseInput op (B.drop 1 rest)
    Nothing        -> case fromJust $ B.uncons bs of
      (' ', rest) -> parseInput op rest
      (')', rest) -> parseInput op rest
      ('+', rest) -> parseInput Add rest
      ('*', rest) -> parseInput Mult rest
      ('(', rest) ->
        (op, Scope (parseInput Add innerScope)) : parseInput op restOfScope
       where
        openBrackets  = zip (B.elemIndices '(' rest) (repeat 1)
        closeBrackets = zip (B.elemIndices ')' rest) (repeat (-1))
        Just (matchingIndex, _) =
          find ((== -1) . snd)
            $ scanl1 (\(_, v1) (i, v2) -> (i, v1 + v2))
            $ merge openBrackets closeBrackets
        (innerScope, restOfScope) = B.splitAt matchingIndex rest

calculate :: [Instruction] -> Int
calculate = calculate' 0
 where
  calculate' total []       = total
  calculate' total (i : is) = case i of
    (_, Literal x) -> calculate' (total `op` x) is
    (_, Scope s  ) -> calculate' (total `op` calculate s) is
   where
    op = case i of
      (Add , _) -> (+)
      (Mult, _) -> (*)

sumTotals :: [[Instruction]] -> Int
sumTotals = sum . map calculate

calculate2 :: [Instruction] -> Int
calculate2 = calculate' 1 0
 where
  calculate' total mult []       = total * mult
  calculate' total mult (i : is) = calculate' total' mult' is
   where
    total' = case fst i of
      Add  -> total
      Mult -> total * mult
    mult' = case fst i of
      Add  -> mult + ownValue
      Mult -> ownValue
    ownValue = case snd i of
      Literal x -> x
      Scope   s -> calculate2 s

sumTotals2 :: [[Instruction]] -> Int
sumTotals2 = sum . map calculate2

loadInput :: String -> IO [[Instruction]]
loadInput fileName =
  map (parseInput Add) . B.lines <$> B.readFile ("src/" ++ fileName)
