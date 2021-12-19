{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  (
  )
where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit)
import qualified Data.Char as A
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Numeric (readInt)

type LiteralPair a = (a, a)

data Pair a = Literal Int (LiteralPair a) | Balanced Int (Pair a, Pair a) | LeftPair Int (Pair a, a) | RightPair Int (a, Pair a)
  deriving (Eq)

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
  left <- digitToInt <$> A.digit
  ","
  right <- digitToInt <$> A.digit
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
  right <- digitToInt <$> A.digit
  "]"
  return $ LeftPair depth (left, right)
  where
    depth' = depth + 1

--- >>> A.parseOnly (parseRightPair 1) "[9,[8,7]]"
-- Right (RightPair 1 (9,Literal 2 (8,7)))
parseRightPair :: Int -> A.Parser (Pair Int)
parseRightPair depth = do
  "["
  left <- digitToInt <$> A.digit
  ","
  right <- parsePair depth'
  "]"
  return $ RightPair depth (left, right)
  where
    depth' = depth + 1

-- >>> readPair "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
-- Balanced 1 (Balanced 2 (RightPair 3 (9,Literal 4 (3,8)),LeftPair 3 (Literal 4 (0,9),6)),LeftPair 2 (Balanced 3 (Literal 4 (3,7),Literal 4 (4,9)),3))
readPair :: BSC.ByteString -> Pair Int
readPair input = result
  where
    Right result = A.parseOnly (parsePair 1) input

-- >>> readPair "[1,2]" `add` readPair "[[3,4],5]"
-- Balanced 1 (Literal 2 (1,2),LeftPair 2 (Literal 3 (3,4),5))
add :: Pair a -> Pair a -> Pair a
add left right = increaseDepth $ Balanced 0 (left, right)
  where
    increaseDepth (Literal d p) = Literal (d + 1) p
    increaseDepth (Balanced d (a, b)) = Balanced (d + 1) (increaseDepth a, increaseDepth b)
    increaseDepth (LeftPair d (p, x)) = LeftPair (d + 1) (increaseDepth p, x)
    increaseDepth (RightPair d (x, p)) = RightPair (d + 1) (x, increaseDepth p)

-- >>> toExplode $ readPair "[[[[[9,8],1],2],3],4]"
-- Just (Literal 5 (9,8))

-- >>> toExplode $ readPair "[7,[6,[5,[4,[3,2]]]]] "
-- Just (Literal 5 (3,2))

-- >>> toExplode $ readPair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- Just (Literal 5 (7,3))
toExplode :: Pair a -> Maybe (Pair a)
toExplode literal@(Literal depth _)
  | depth > 4 = Just literal
  | otherwise = Nothing
toExplode (LeftPair _ (p, x)) = toExplode p
toExplode (RightPair _ (x, p)) = toExplode p
toExplode (Balanced _ (a, b)) = head (map Just (mapMaybe toExplode [a, b]) ++ [Nothing])

-- >>> toExplode $ readPair "[[[[[9,8],1],2],3],4]"

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

-- >>> explode $ readPair "[[[[[9,8],1],2],3],4]"
-- (Just (0,(9,0)),[[[[0,9],2],3],4])

-- >>> explode $ readPair "[7,[6,[5,[4,[3,2]]]]]"
-- (Just (3,(0,3)),[7,[6,[5,[7,0]]]])

-- >>> explode $ readPair "[[6,[5,[4,[3,2]]]],1]"
-- (Just (2,(0,3)),[[6,[5,[7,0]]],1])

-- >>> size $ readPair "[1,[[[[3,2],4],5],6]]"
-- 1
size :: Pair a -> Int
size (Literal d _) = 1
size (Balanced d (l, r)) = size l + size r
size (LeftPair d (p, x)) = size p + 1
size (RightPair d (x, p)) = size p + 1

-- >>> explode $ readPair "[1,[[[[3,2],4],5],6]]"
-- [4,[[[0,6],5],6]]
toIndex :: Pair a -> Pair Int
toIndex = toIndex' 0
  where
    toIndex' current literal@(Literal d (x, y)) = Literal d (current, current)
    toIndex' current leftPair@(LeftPair d (p, y)) = LeftPair d (toIndex' (current) p, current + (size p))
    toIndex' current rightPair@(RightPair d (x, p)) = RightPair d (current, toIndex' (current + 1) p)
    toIndex' current (Balanced d (l, r)) = Balanced d (toIndex' current l, toIndex' (current + size l) r)

-- >>> toIndex $ readPair "[1,[[[[3,2],4],5],6]]"
-- [0,[[[[1,1],2],3],4]]

-- >>> toIndex $ readPair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- [[0,[1,[2,[3,3]]]],[4,[5,[6,[7,7]]]]]

-- >>> fmapIndex (const 99, const 88) 2 $ readPair "[1,[[[[3,2],4],5],6]]"
-- [1,[[[[3,2],88],5],6]]

fmapIndex :: (a -> a) -> Int -> Pair a -> Pair a
fmapIndex f n = fmapIndex' 0
  where
    fmapIndex' current literal@(Literal d (x, y))
      | current == n = Literal d (f x, f y)
      | otherwise = literal
    fmapIndex' current leftPair@(LeftPair d (p, y))
      | current + size p == n = LeftPair d (fmapIndex' current p, f y)
      | otherwise = LeftPair d (fmapIndex' current p, y)
    fmapIndex' current rightPair@(RightPair d (x, p))
      | current == n = RightPair d (f x, fmapIndex' (current + 1) p)
      | otherwise = RightPair d (x, fmapIndex' (current + 1) p)
    fmapIndex' current (Balanced d (l, r)) = Balanced d (fmapIndex' current l, fmapIndex' (current + size l) r)

explode :: (Eq a, Num a) => Pair a -> Pair a
explode p = case exploded of
  (Just (index, (lCarry, rCarry)), p) -> fmapIndex (+ rCarry) (index + 1) $ fmapIndex (+ lCarry) (index - 1) $ p
  _ -> p
  where
    e = toExplode p
    exploded = explodeAt' e 0 Nothing p

-- explodeAt (Just toExplode) currentIndex (Just ExplodedIndex) () ->
explodeAt' :: Eq a => Num a => Maybe (Pair a) -> Int -> (Maybe (Int, (a, a))) -> Pair a -> (Maybe (Int, (a, a)), Pair a)
explodeAt' Nothing _ _ p = (Nothing, p)
explodeAt' _ _ index@(Just _) p = (index, p)
explodeAt' _ i _ literal@(Literal _ _) = (Nothing, literal)
explodeAt' e i _ leftPair@(LeftPair _ (literal@(Literal d (x, y)), z))
  | Just literal == e = (Just (i, (x, 0)), Literal (d - 1) (0, y + z))
  | otherwise = (Nothing, leftPair)
explodeAt' e i _ rightPair@(RightPair _ (x, literal@(Literal d (y, z))))
  | Just literal == e = (Just (i, (0, z)), Literal (d - 1) (x + y, 0))
  | otherwise = (Nothing, rightPair)
explodeAt' e i _ balanced@(Balanced d (l, r))
  | Just l == e = (Just (i, (leftMost l, 0)), r)
  | Just r == e = (Just (i, (0, rightMost r)), l)
  | otherwise = case explodeAt' e i Nothing l of
    (index@(Just _), l') -> (index, Balanced d (l', r))
    _ -> case explodeAt' e (i + size l) Nothing r of
      (index@(Just _), r') -> (index, Balanced d (l, r'))
      _ -> (Nothing, balanced)
explodeAt' e i _ leftPair@(LeftPair d (l, x)) = case explodeAt' e i Nothing l of
  (index@(Just _), l') -> (index, LeftPair d (l', x))
  (Nothing, _) -> (Nothing, leftPair)
explodeAt' e i _ rightPair@(RightPair d (x, r)) = case explodeAt' e (i + 1) Nothing r of
  (index@(Just _), r') -> (index, RightPair d (x, r'))
  (Nothing, _) -> (Nothing, rightPair)

-- >>> explode $ readPair "[[[[[9,8],1],2],3],4]"
-- [[[[0,9],2],3],4]

-- >>> explode $ readPair "[7,[6,[5,[4,[3,2]]]]]"
-- [7,[6,[5,[7,0]]]]

-- >>> explode $ readPair "[[6,[5,[4,[3,2]]]],1]"
-- [[6,[5,[7,0]]],3]

-- >>> explode $ readPair "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]

-- >>> explode $ readPair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
-- [[3,[2,[8,0]]],[9,[5,[7,0]]]]

-- >>> (\x -> explodeAt' (toExplode x) 0 Nothing x) $ readPair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
-- (Just (3,(0,2)),[[3,[2,[8,0]]],[9,[5,[7,0]]]])

-- >>> toIndex $ readPair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
-- [[0,[1,[2,2]]],[3,[4,[5,[6,6]]]]]
