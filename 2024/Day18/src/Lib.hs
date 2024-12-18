{-# LANGUAGE GADTs #-}
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
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.PSQueue (Binding ((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set qualified as S

type Coord2D = (Int, Int)

data Input where
  Input ::
    {_bounds :: Coord2D, _bytes :: [Coord2D]} ->
    Input
  deriving (Show)

-- >>> A.parseOnly parseCoord "5,4"
-- Right (5,4)
parseCoord :: A.Parser Coord2D
parseCoord = (,) <$> A.decimal <* A.char ',' <*> A.decimal

parseCoords :: A.Parser [Coord2D]
parseCoords = parseCoord `A.sepBy` A.endOfLine

-- >>> (`fallenAtTime` 12) <$> loadInput (6, 6) "example.txt"
-- fromList [(0,6),(1,5),(2,1),(2,4),(2,6),(3,0),(3,3),(4,2),(4,5),(5,1),(5,4),(6,3)]
fallenAtTime :: Input -> Int -> S.Set Coord2D
fallenAtTime s t = S.fromList $ take t $ _bytes s

-- >>> loadInput (6, 6) "example.txt"
-- State {_bounds = (6,6), _bytes = [(5,4),(4,2),(4,5),(3,0),(2,1),(6,3),(2,4),(1,5),(0,6),(3,3),(2,6),(5,1),(1,2),(5,5),(2,5),(6,5),(1,4),(0,4),(6,4),(1,1),(6,1),(1,0),(0,5),(1,6),(2,0)]}
loadInput :: Coord2D -> [Char] -> IO Input
loadInput bounds = (Input bounds . fromRight [] . A.parseOnly parseCoords <$>) . BSC.readFile . ("src/" ++)

dijkstra :: Coord2D -> S.Set Coord2D -> M.Map Coord2D Int
dijkstra (maxX, maxY) corrupted = go M.empty $ PSQ.fromList [(0, 0) :-> 0]
  where
    inBounds (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
    go costs edges = case PSQ.minView edges of
      Nothing -> costs
      Just (edge@(x, y) :-> cost, edges') -> case M.lookup edge costs of
        Just _ -> go costs edges'
        Nothing ->
          let neighbours = S.filter inBounds $ S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] S.\\ corrupted
              edges'' = S.foldl (\acc e -> PSQ.insert e (cost + 1) acc) edges' neighbours
              costs' = M.insert edge cost costs
           in go costs' edges''

-- >>> part1 12 <$> loadInput (6, 6) "example.txt"
-- Just 22
part1 :: Int -> Input -> Maybe Int
part1 t i@(Input bounds _) = bounds `M.lookup` dijkstra bounds (i `fallenAtTime` t)

-- >>> part2  <$> loadInput (6,6) "example.txt"
-- (6,1)
part2 :: Input -> Coord2D
part2 i = fst $ head $ dropWhile (isJust . snd) $ zipWith (\t b -> (b, part1 t i)) [1 ..] (_bytes i)
