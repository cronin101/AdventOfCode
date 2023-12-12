{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S

type Coord2D = (Int, Int)

type Space = S.Set Coord2D

-- >>> A.parseOnly parseSpaceRow "...#......"
-- Right [Nothing,Nothing,Nothing,Just (Star {location = (3,0)}),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
parseSpaceRow :: A.Parser [Maybe Coord2D]
parseSpaceRow = A.many1' (A.choice ["." $> Nothing, "#" $> Just (0, 0)])

parseSpace :: A.Parser Space
parseSpace =
  S.fromList
    . catMaybes
    . concat
    . addYCoords
    . addXCoords
    <$> (parseSpaceRow `A.sepBy1` A.endOfLine)
  where
    addYCoords = zipWith (\y row -> map (fmap (\(x, _) -> (x, y))) row) [0 ..] . reverse
    addXCoords = map (zipWith (\x e -> fmap (\(_, y) -> (x, y)) e) [0 ..])

-- >>> loadInput "example.txt"
-- fromList [((0,2),Star {location = (0,2)}),((0,11),Star {location = (0,11)}),((1,6),Star {location = (1,6)}),((4,0),Star {location = (4,0)}),((5,11),Star {location = (5,11)}),((8,5),Star {location = (8,5)}),((9,1),Star {location = (9,1)}),((9,10),Star {location = (9,10)}),((12,7),Star {location = (12,7)})]
loadInput :: [Char] -> IO Space
loadInput = (fromRight S.empty . A.parseOnly parseSpace <$>) . BSC.readFile . ("src/" ++)

galaxyDistance :: Coord2D -> Coord2D -> Int
galaxyDistance (x1, y1) (x2, y2) = abs (max x1 x2 - min x1 x2) + abs (max y1 y2 - min y1 y2)

spaceDistance :: Space -> Int
spaceDistance s = sum [galaxyDistance a b | a <- S.toList s, b <- S.toList s, a < b]

minX :: Space -> Int
minX space = S.findMin $ S.map fst space

maxX :: Space -> Int
maxX space = S.findMax $ S.map fst space

minY :: Space -> Int
minY space = S.findMin $ S.map snd space

maxY :: Space -> Int
maxY space = S.findMax $ S.map snd space

emptyRows :: Space -> S.Set Int
emptyRows s = S.fromList [minY s .. maxY s] S.\\ S.map snd s

emptyColumns :: Space -> S.Set Int
emptyColumns s = S.fromList [minX s .. maxX s] S.\\ S.map fst s

expandSpace :: Int -> Space -> Space
expandSpace n space = S.map (\(x, y) -> (x + xExpansion x, y + yExpansion y)) space
  where
    xExpansion x = (n - 1) * S.size (S.filter (< x) $ emptyColumns space)
    yExpansion y = (n - 1) * S.size (S.filter (< y) $ emptyRows space)

-- >>> part1 <$> loadInput "example.txt"
-- 374
part1 :: Space -> Int
part1 = spaceDistance . expandSpace 2

-- >>> spaceDistance . expandSpace 100 <$> loadInput "example.txt"
-- 8410

-- >>> part2 <$> loadInput "input.txt"
-- 553083047914
part2 :: Space -> Int
part2 = spaceDistance . expandSpace 1000000

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [fromMaybe "." (M.lookup (x, y) m) | x <- [0 .. maxX keys]] | y <- [maxY keys, maxY keys - 1 .. 0]])
  where
    keys = M.keysSet m

printSpace :: Space -> String
printSpace = printGrid . M.fromList . map (,"#") . S.toList
