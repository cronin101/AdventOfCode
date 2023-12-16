{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S

type Coord2D = (Int, Int)

type Grid = M.Map Coord2D Entity

data CardinalDirections = North | East | South | West deriving (Show, Eq, Ord)

type Beam = (Coord2D, CardinalDirections)

data Entity
  = ForwardMirror
  | BackwardMirror
  | HorizontalSplitter
  | VerticalSplitter
  deriving (Show, Eq, Ord)

parseEntity :: A.Parser Entity
parseEntity = "/" $> ForwardMirror <|> "\\" $> BackwardMirror <|> "-" $> HorizontalSplitter <|> "|" $> VerticalSplitter

parseMaybeEntity :: A.Parser (Maybe Entity)
parseMaybeEntity = A.choice [Just <$> parseEntity, "." $> Nothing]

-- >>> A.parseOnly parseRow ".-.-/..|.."
-- Right [((1,0),HorizontalSplitter),((3,0),HorizontalSplitter),((4,0),ForwardMirror),((7,0),VerticalSplitter)]
parseRow :: A.Parser [(Coord2D, Entity)]
parseRow = catMaybes . zipWith (\x e -> fmap ((x, 0),) e) [0 ..] <$> A.many1 parseMaybeEntity

parseGrid :: A.Parser Grid
parseGrid = M.fromList . concat . zipWith (\y r -> map (\((x, _), e) -> ((x, y), e)) r) [0 ..] . reverse <$> parseRow `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- fromList [((0,8),VerticalSplitter),((1,1),VerticalSplitter),((1,2),HorizontalSplitter),((1,9),VerticalSplitter),((2,0),ForwardMirror),((2,8),HorizontalSplitter),((3,0),ForwardMirror),((3,2),HorizontalSplitter),((4,2),ForwardMirror),((4,3),ForwardMirror),((4,8),BackwardMirror),((5,0),VerticalSplitter),((5,7),VerticalSplitter),((5,9),BackwardMirror),((6,1),HorizontalSplitter),((6,3),BackwardMirror),((6,7),HorizontalSplitter),((7,1),VerticalSplitter),((7,2),VerticalSplitter),((7,3),BackwardMirror),((8,6),VerticalSplitter),((9,1),BackwardMirror),((9,4),BackwardMirror)]
loadInput :: [Char] -> IO Grid
loadInput = (fromRight M.empty . A.parseOnly parseGrid <$>) . BSC.readFile . ("src/" ++)

bounds :: Grid -> (Coord2D, Coord2D)
bounds grid = ((xMin, yMin), (xMax, yMax))
  where
    xMin = minimum $ map fst $ M.keys grid
    xMax = maximum $ map fst $ M.keys grid
    yMin = minimum $ map snd $ M.keys grid
    yMax = maximum $ map snd $ M.keys grid

nextBeamInVacuum :: Beam -> Beam
nextBeamInVacuum ((x, y), North) = ((x, y + 1), North)
nextBeamInVacuum ((x, y), East) = ((x + 1, y), East)
nextBeamInVacuum ((x, y), South) = ((x, y - 1), South)
nextBeamInVacuum ((x, y), West) = ((x - 1, y), West)

collision :: Entity -> Beam -> [Beam]
collision ForwardMirror (c, North) = pure (c, East)
collision ForwardMirror (c, East) = pure (c, North)
collision ForwardMirror (c, South) = pure (c, West)
collision ForwardMirror (c, West) = pure (c, South)
collision BackwardMirror (c, North) = pure (c, West)
collision BackwardMirror (c, West) = pure (c, North)
collision BackwardMirror (c, South) = pure (c, East)
collision BackwardMirror (c, East) = pure (c, South)
collision HorizontalSplitter (c, North) = [(c, East), (c, West)]
collision HorizontalSplitter (c, South) = [(c, East), (c, West)]
collision HorizontalSplitter (c, d) = pure (c, d)
collision VerticalSplitter (c, East) = [(c, North), (c, South)]
collision VerticalSplitter (c, West) = [(c, North), (c, South)]
collision VerticalSplitter (c, d) = pure (c, d)

rayTrace :: Grid -> Beam -> S.Set Coord2D
rayTrace grid (start, direction) = solve (S.singleton (start, direction)) S.empty
  where
    ((xMin, yMin), (xMax, yMax)) = bounds grid
    solve frontier visited
      | S.null frontier = S.map fst visited
      | otherwise = solve frontier' visited'
      where
        frontier' = S.unions (S.map nextBeam frontier) S.\\ visited
        nextBeam = S.filter inBounds . S.fromList . maybeCollide . nextBeamInVacuum
        inBounds ((x, y), _) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
        visited' = S.union frontier' visited
        maybeCollide beam = case M.lookup (fst beam) grid of
          Just e -> collision e beam
          Nothing -> pure beam

-- >>> part1 <$> loadInput "example.txt"
-- 46
part1 :: Grid -> Int
part1 grid = S.size $ rayTrace grid ((xMin - 1, yMax), East)
  where
    ((xMin, _), (_, yMax)) = bounds grid

-- >>> part2 <$> loadInput "example.txt"
-- 51
part2 :: Grid -> Int
part2 grid = maximum $ map (S.size . rayTrace grid) beams
  where
    beams = leftEdge ++ topEdge ++ rightEdge ++ bottomEdge
    ((xMin, yMin), (xMax, yMax)) = bounds grid
    leftEdge = [((xMin - 1, y), East) | y <- [yMin .. yMax]]
    rightEdge = [((xMax + 1, y), West) | y <- [yMin .. yMax]]
    topEdge = [((x, yMax + 1), South) | x <- [xMin .. xMax]]
    bottomEdge = [((x, yMax + 1), North) | x <- [xMin .. xMax]]
