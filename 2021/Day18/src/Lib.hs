{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( readPair,
    explode,
    add,
    explodeAt',
    toExplode,
    reduce,
    split,
    reduceMany,
    loadInput,
    magnitude',
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second), first)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit)
import qualified Data.Char as A
import Data.Complex (magnitude)
import Data.Either (fromRight, isRight)
import Data.Function (fix, on)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Numeric (readInt)

type LiteralPair a = (a, a)

data Pair a = Literal Int (LiteralPair a) | Balanced Int (Pair a, Pair a) | LeftPair Int (Pair a, a) | RightPair Int (a, Pair a)
  deriving (Eq, Functor)

instance Show a => Show (Pair a) where
  show (Literal _ (x, y)) = show [x, y]
  show (LeftPair _ (l, i)) = "[" ++ show l ++ "," ++ show i ++ "]"
  show (RightPair _ (i, r)) = "[" ++ show i ++ "," ++ show r ++ "]"
  show (Balanced _ (l, r)) = "[" ++ show l ++ "," ++ show r ++ "]"

-- >>> A.parseOnly  (parseLiteralPair 1) "[1,2]"
-- Right (Literal 1 (1,2))
parseLiteralPair :: Int -> A.Parser (Pair Int)
parseLiteralPair depth = do
  "["
  left <- A.decimal
  ","
  right <- A.decimal
  "]"
  return $ Literal depth (left, right)
  where
    depth' = depth + 1

-- >>> A.parseOnly (parsePair 1) "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
-- Right (Balanced 1 (Balanced 2 (Balanced 3 (Literal 4 (1,3),Literal 4 (5,3)),Balanced 3 (Literal 4 (1,3),Literal 4 (8,7))),Balanced 2 (Balanced 3 (Literal 4 (4,9),Literal 4 (6,9)),Balanced 3 (Literal 4 (8,2),Literal 4 (7,3)))))
parsePair :: Int -> A.Parser (Pair Int)
parsePair depth = parseLiteralPair depth <|> parseBalancedPair depth <|> parseLeftPair depth <|> parseRightPair depth

-- >>> A.parseOnly (parseBalancedPair 1) "[[1,9],[8,5]]"
-- Right (Balanced 1 (Literal 2 (1,9),Literal 2 (8,5)))
parseBalancedPair :: Int -> A.Parser (Pair Int)
parseBalancedPair depth = do
  "["
  left <- parsePair depth'
  ","
  right <- parsePair depth'
  "]"
  return $ Balanced depth (left, right)
  where
    depth' = depth + 1

-- >>> A.parseOnly (parseLeftPair 1) "[[1,2],3]"
-- Right (LeftPair 1 (Literal 2 (1,2),3))
parseLeftPair :: Int -> A.Parser (Pair Int)
parseLeftPair depth = do
  "["
  left <- parsePair depth'
  ","
  right <- A.decimal
  "]"
  return $ LeftPair depth (left, right)
  where
    depth' = depth + 1

--- >>> A.parseOnly (parseRightPair 1) "[9,[8,7]]"
-- Right (RightPair 1 (9,Literal 2 (8,7)))
parseRightPair :: Int -> A.Parser (Pair Int)
parseRightPair depth = do
  "["
  left <- A.decimal
  ","
  right <- parsePair depth'
  "]"
  return $ RightPair depth (left, right)
  where
    depth' = depth + 1

parseInput :: A.Parser [Pair Int]
parseInput = A.sepBy1 (parsePair 1) A.endOfLine

-- >>> readPair "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
-- [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
readPair :: BSC.ByteString -> Pair Int
readPair input = result
  where
    Right result = A.parseOnly (parsePair 1) input

-- >>> readPair "[1,2]" `add` readPair "[[3,4],5]"
-- [[1,2],[[3,4],5]]
add :: Pair a -> Pair a -> Pair a
add left right = increaseDepth $ Balanced 0 (left, right)
  where
    increaseDepth (Literal d p) = Literal (d + 1) p
    increaseDepth (Balanced d (a, b)) = Balanced (d + 1) (increaseDepth a, increaseDepth b)
    increaseDepth (LeftPair d (p, x)) = LeftPair (d + 1) (increaseDepth p, x)
    increaseDepth (RightPair d (x, p)) = RightPair (d + 1) (x, increaseDepth p)

toExplode :: Pair Int -> Maybe (Int, Pair Int)
toExplode p = case toExplode' $ withIndex p of
  Just (Literal d (x, y)) -> Just (fst x, Literal d (snd x, snd y))
  _ -> Nothing

-- >>> toExplode $ readPair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- Just (3,[7,3])
toExplode' :: Pair a -> Maybe (Pair a)
toExplode' literal@(Literal depth _)
  | depth > 4 = Just literal
  | otherwise = Nothing
toExplode' (LeftPair _ (p, x)) = toExplode' p
toExplode' (RightPair _ (x, p)) = toExplode' p
toExplode' (Balanced _ (a, b)) = head (map Just (mapMaybe toExplode' [a, b]) ++ [Nothing])

leftMost :: Pair a -> a
leftMost (Literal _ (l, _)) = l
leftMost (RightPair _ (l, _)) = l
leftMost (LeftPair _ (l, _)) = leftMost l
leftMost (Balanced _ (l, _)) = leftMost l

rightMost :: Pair a -> a
rightMost (Literal _ (_, r)) = r
rightMost (RightPair _ (_, r)) = rightMost r
rightMost (LeftPair _ (_, r)) = r
rightMost (Balanced _ (_, r)) = rightMost r

-- >>> size $ readPair "[1,[[[[3,2],4],5],6]]"
-- 1
size :: Pair a -> Int
size (Literal d _) = 1
size (Balanced d (l, r)) = size l + size r
size (LeftPair d (p, x)) = size p + 1
size (RightPair d (x, p)) = size p + 1

-- >>> withIndex $ readPair "[1,[[[[3,2],4],5],6]]"
-- [(0,1),[[[[(1,3),(1,2)],(2,4)],(3,5)],(4,6)]]
withIndex :: Pair Int -> Pair (Int, Int)
withIndex = toIndex' 0
  where
    toIndex' current literal@(Literal d (x, y)) = Literal d ((current, x), (current, y))
    toIndex' current leftPair@(LeftPair d (p, y)) = LeftPair d (toIndex' current p, (current + size p, y))
    toIndex' current rightPair@(RightPair d (x, p)) = RightPair d ((current, x), toIndex' (current + 1) p)
    toIndex' current (Balanced d (l, r)) = Balanced d (toIndex' current l, toIndex' (current + size l) r)

toIndex :: Pair Int -> Pair Int
toIndex p = fst <$> withIndex p

-- >>> toIndex $ readPair "[1,[[[[3,2],4],5],6]]"
-- [0,[[[[1,1],2],3],4]]

-- >>> toIndex $ readPair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- [[0,[1,[2,[3,3]]]],[4,[5,[6,[7,7]]]]]

fmapIndex :: (a -> a) -> (LiteralPair a -> LiteralPair a) -> Int -> Pair a -> Pair a
fmapIndex f fl n = fmapIndex' 0
  where
    fmapIndex' current literal@(Literal d l)
      | current == n = Literal d (fl l)
      | otherwise = literal
    fmapIndex' current leftPair@(LeftPair d (p, y))
      | current + size p == n = LeftPair d (p, f y)
      | otherwise = LeftPair d (fmapIndex' current p, y)
    fmapIndex' current rightPair@(RightPair d (x, p))
      | current == n = RightPair d (f x, p)
      | otherwise = RightPair d (x, fmapIndex' (current + 1) p)
    fmapIndex' current (Balanced d (l, r))
      | n < current + size l = Balanced d (fmapIndex' current l, r)
      | otherwise = Balanced d (l, fmapIndex' (current + size l) r)

explode :: Pair Int -> Pair Int
explode p = case exploded of
  (Just ((lIndex, rIndex), (lCarry, rCarry)), p) -> fmapIndex (+ rCarry) (first (+ rCarry)) rIndex $ fmapIndex (+ lCarry) (second (+ lCarry)) lIndex p
  _ -> p
  where
    e = toExplode p
    exploded = explodeAt' e 0 Nothing p

-- explodeAt' (Just (index, toExplode)) currentIndex (Just ((leftCarryIndex, rightCarryIndex),(lCarry,rCarry)) pair
explodeAt' :: Eq a => Num a => Maybe (Int, Pair a) -> Int -> Maybe ((Int, Int), (a, a)) -> Pair a -> (Maybe ((Int, Int), (a, a)), Pair a)
explodeAt' Nothing _ _ p = (Nothing, p)
explodeAt' _ _ index@(Just _) p = (index, p)
explodeAt' _ i _ literal@(Literal _ _) = (Nothing, literal)
explodeAt' e i _ leftPair@(LeftPair _ (literal@(Literal d (x, y)), z))
  | Just (i, literal) == e = (Just ((i - 1, i), (x, 0)), Literal (d - 1) (0, y + z))
  | otherwise = (Nothing, leftPair)
explodeAt' e i _ rightPair@(RightPair _ (x, literal@(Literal d (y, z))))
  | Just (i + 1, literal) == e = (Just ((i, i + 1), (0, z)), Literal (d - 1) (x + y, 0))
  | otherwise = (Nothing, rightPair)
explodeAt' e i _ balanced@(Balanced d (l, r))
  | Just (i, l) == e = (Just ((i -1, i + 1), (leftMost l, rightMost l)), RightPair d (0, r))
  | Just (i + size l, r) == e = (Just ((i + size l - 1, i + size l + 1), (leftMost r, rightMost r)), LeftPair d (l, 0))
  | otherwise = case explodeAt' e i Nothing l of
    (index@(Just _), l') -> (index, Balanced d (l', r))
    _ -> case explodeAt' e (i + size l) Nothing r of
      (index@(Just (i, diff)), r') -> (Just (i, diff), Balanced d (l, r'))
      _ -> (Nothing, balanced)
explodeAt' e i _ leftPair@(LeftPair d (l, x)) = case explodeAt' e i Nothing l of
  (index@(Just (i', diff)), l') -> (Just (i', diff), LeftPair d (l', x))
  (Nothing, _) -> (Nothing, leftPair)
explodeAt' e i _ rightPair@(RightPair d (x, r)) = case explodeAt' e (i + 1) Nothing r of
  (index@(Just (i', diff)), r') -> (Just (i', diff), RightPair d (x, r'))
  (Nothing, _) -> (Nothing, rightPair)

splitValue :: (Num a, Integral a) => Int -> a -> Pair a
splitValue d x = Literal (d + 1) (floor (x `divide` 2), ceiling (x `divide` 2))
  where
    divide = (/) `on` fromIntegral

-- >>>  split  $ readPair "[[[[0,7],4],[15,[0,13]]],[1,1]]"
-- [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
split :: (Num a, Integral a) => Ord a => Pair a -> Pair a
split literal@(Literal d (x, y))
  | x >= 10 = LeftPair d (splitValue d x, y)
  | y >= 10 = RightPair d (x, splitValue d y)
  | otherwise = literal
split (LeftPair d (p, y))
  | y >= 10 && split p == p = Balanced d (p, splitValue d y)
  | otherwise = LeftPair d (split p, y)
split (RightPair d (x, p))
  | x >= 10 = Balanced d (splitValue d x, p)
  | otherwise = RightPair d (x, split p)
split (Balanced d (a, b))
  | split a /= a = Balanced d (split a, b)
  | otherwise = Balanced d (a, split b)

-- >>> reduce $ readPair "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
-- [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
reduce :: Pair Int -> Pair Int
reduce = (until =<< ((==) =<<)) step
  where
    step = split . (until =<< ((==) =<<)) explode

-- >>> loadInput "example.txt"
-- [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]
loadInput fileName =
  fromRight
    [Literal 0 (0, 0)]
    . A.parseOnly
      parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> magnitude' . reduceMany <$> loadInput "input.txt"
-- 4120
reduceMany :: [Pair Int] -> Pair Int
reduceMany = foldl1 ((reduce .) . add)

magnitude' :: Pair Int -> Int
magnitude' (Literal _ (x, y)) = 3 * x + 2 * y
magnitude' (LeftPair _ (p, x)) = 3 * magnitude' p + 2 * x
magnitude' (RightPair _ (x, p)) = 3 * x + 2 * magnitude' p
magnitude' (Balanced _ (a, b)) = 3 * magnitude' a + 2 * magnitude' b
