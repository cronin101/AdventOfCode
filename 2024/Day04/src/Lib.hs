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
import Control.Arrow (first)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

type Coord2D = (Int, Int)

type Crossword = M.Map Coord2D Char

-- Parses a single row of the crossword grid.
-- >>> A.parseOnly parseRow "MMMSXXMASM"
-- Right [((0,0),'M'),((1,0),'M'),((2,0),'M'),((3,0),'S'),((4,0),'X'),((5,0),'X'),((6,0),'M'),((7,0),'A'),((8,0),'S'),((9,0),'M')]
parseRow :: A.Parser [(Coord2D, Char)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] . map BSC.head <$> A.many1 ("X" <|> "M" <|> "A" <|> "S")

-- Parses the entire crossword grid.
-- It combines all rows into a single map of coordinates and characters.
parseCrossword :: A.Parser Crossword
parseCrossword = M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)

-- Finds all linear occurrences of the word "XMAS" in the crossword.
-- It searches in all possible directions and filters out the correct spellings.
-- >>> (linearOccurrences "XMAS") <$> loadInput ("example.txt")
-- [[((0,4),'X'),((1,5),'M'),((2,6),'A'),((3,7),'S')],[((0,5),'X'),((1,5),'M'),((2,5),'A'),((3,5),'S')],[((1,0),'X'),((2,1),'M'),((3,2),'A'),((4,3),'S')],[((3,0),'X'),((4,1),'M'),((5,2),'A'),((6,3),'S')],[((3,0),'X'),((2,1),'M'),((1,2),'A'),((0,3),'S')],[((4,8),'X'),((3,8),'M'),((2,8),'A'),((1,8),'S')],[((4,9),'X'),((5,8),'M'),((6,7),'A'),((7,6),'S')],[((5,0),'X'),((6,0),'M'),((7,0),'A'),((8,0),'S')],[((5,0),'X'),((6,1),'M'),((7,2),'A'),((8,3),'S')],[((5,0),'X'),((4,1),'M'),((3,2),'A'),((2,3),'S')],[((5,9),'X'),((6,9),'M'),((7,9),'A'),((8,9),'S')],[((6,4),'X'),((5,5),'M'),((4,6),'A'),((3,7),'S')],[((6,5),'X'),((6,6),'M'),((6,7),'A'),((6,8),'S')],[((6,5),'X'),((5,5),'M'),((4,5),'A'),((3,5),'S')],[((9,0),'X'),((9,1),'M'),((9,2),'A'),((9,3),'S')],[((9,0),'X'),((8,1),'M'),((7,2),'A'),((6,3),'S')],[((9,6),'X'),((9,5),'M'),((9,4),'A'),((9,3),'S')],[((9,6),'X'),((8,5),'M'),((7,4),'A'),((6,3),'S')]]
linearOccurrences :: String -> Crossword -> [[(Coord2D, Char)]]
linearOccurrences s crossword = filter (isMatch s) $ concatMap (allTraversalsOfLength (length s)) $ findAll (head s) crossword
  where
    isMatch word = (== word) . map snd
    allTraversalsOfLength n = linearTraversals n crossword

-- Generates all possible linear traversals of a given length from a starting coordinate.
-- It searches in all possible directions and filters out the traversals of the correct length.
-- >>>  (\c -> linearTraversals 4 c (0,0)) <$> loadInput ("example.txt")
-- [[((0,0),'M'),((1,0),'X'),((2,0),'M'),((3,0),'X')],[((0,0),'M'),((0,1),'M'),((0,2),'S'),((0,3),'S')],[((0,0),'M'),((1,1),'A'),((2,2),'X'),((3,3),'M')]]
linearTraversals :: Int -> Crossword -> Coord2D -> [[(Coord2D, Char)]]
linearTraversals len crossword (x, y) =
  filter ((== len) . length) $
    map
      (mapMaybe (\coord -> (coord,) <$> (coord `M.lookup` crossword)))
      [ [(x + i, y) | i <- offsets],
        [(x, y + i) | i <- offsets],
        [(x - i, y) | i <- offsets],
        [(x, y - i) | i <- offsets],
        [(x + i, y + i) | i <- offsets],
        [(x - i, y - i) | i <- offsets],
        [(x + i, y - i) | i <- offsets],
        [(x - i, y + i) | i <- offsets]
      ]
  where
    offsets = [0 .. len - 1]

-- Finds all coordinates of a specific character in the crossword.
-- It returns a list of coordinates where the character is found.
-- >>> findAll 'A' <$> loadInput ("example.txt")
-- [((0,7),'A'),((1,1),'A'),((1,2),'A'),((2,4),'A'),((2,5),'A'),((2,6),'A'),((2,8),'A'),((3,2),'A'),((4,0),'A'),((4,5),'A'),((4,6),'A'),((5,2),'A'),((5,3),'A'),((6,7),'A'),((7,0),'A'),((7,2),'A'),((7,4),'A'),((7,5),'A'),((7,7),'A'),((7,9),'A'),((8,2),'A'),((9,2),'A'),((9,4),'A'),((9,8),'A')]
findAll :: Char -> Crossword -> [Coord2D]
findAll c = M.keys . M.filter (== c)

-- Finds all occurrences of the X-MAS pattern in the crossword.
-- It searches for the pattern in all possible locations and filters out the correct ones.
-- >>> starMasOccurrences <$> loadInput ("example.txt")
-- [[((0,3),'S'),((2,3),'S'),((1,2),'A'),((0,1),'M'),((2,1),'M')],[((1,7),'M'),((3,7),'S'),((2,6),'A'),((1,5),'M'),((3,5),'S')],[((1,9),'M'),((3,9),'S'),((2,8),'A'),((1,7),'M'),((3,7),'S')],[((2,3),'S'),((4,3),'S'),((3,2),'A'),((2,1),'M'),((4,1),'M')],[((3,7),'S'),((5,7),'M'),((4,6),'A'),((3,5),'S'),((5,5),'M')],[((4,3),'S'),((6,3),'S'),((5,2),'A'),((4,1),'M'),((6,1),'M')],[((5,8),'M'),((7,8),'M'),((6,7),'A'),((5,6),'S'),((7,6),'S')],[((6,3),'S'),((8,3),'S'),((7,2),'A'),((6,1),'M'),((8,1),'M')],[((6,8),'S'),((8,8),'S'),((7,7),'A'),((6,6),'M'),((8,6),'M')]]
starMasOccurrences :: Crossword -> [[(Coord2D, Char)]]
starMasOccurrences crossword = filter isStarMas $ mapMaybe (starTraversal crossword) $ findAll 'A' crossword

-- Verifies if a given set of coordinates and characters form the X-MAS pattern.
-- It ensures the pattern has the correct number of each character and the correct alignment.
isStarMas :: [(Coord2D, Char)] -> Bool
isStarMas pieces = all ($ pieces) ['S' `occurs` 2, 'M' `occurs` 2, 'A' `occurs` 1, isCenter 'A', notDiagonal 'M']
  where
    (minX, maxX) = (\xs -> (minimum xs, maximum xs)) $ map (fst . fst) pieces
    (minY, maxY) = (\xs -> (minimum xs, maximum xs)) $ map (snd . fst) pieces
    yAligned = (1 ==) . length . nub . map snd
    xAligned = (1 ==) . length . nub . map fst
    notDiagonal c = (\cs -> xAligned cs || yAligned cs) . map fst . filter ((== c) . snd)
    isCenter c = all ((\(x, y) -> minX < x && maxX > x && minY < y && maxY > y) . fst) . filter ((== c) . snd)
    occurs c n = (== n) . length . filter ((== c) . snd)

-- Finds a specific traversal pattern in the crossword.
-- It checks if the pattern has the correct length and returns the coordinates and characters.
-- >>>  (\c -> starTraversal c (2, 2)) <$> loadInput ("example.txt")
-- Just [((1,3),'M'),((3,3),'M'),((2,2),'X'),((1,1),'A'),((3,1),'M')]
starTraversal :: Crossword -> Coord2D -> Maybe [(Coord2D, Char)]
starTraversal crossword (x, y) =
  onlyIfLength 5 $
    mapMaybe (\coord -> (coord,) <$> (coord `M.lookup` crossword)) [(x - 1, y + 1), (x + 1, y + 1), (x, y), (x - 1, y - 1), (x + 1, y - 1)]
  where
    onlyIfLength n l = if length l == n then Just l else Nothing

-- Counts the total number of XMAS occurrences in the crossword.
-- It uses the linearOccurrences function to find and count all occurrences.
-- >>> part1 <$> loadInput "example.txt"
-- 18
part1 :: Crossword -> Int
part1 = length . linearOccurrences "XMAS"

-- Counts the total number of X-MAS patterns in the crossword.
-- It uses the starMasOccurrences function to find and count all patterns.
-- >>> part2 <$> loadInput "example.txt"
-- 9
part2 :: Crossword -> Int
part2 = length . starMasOccurrences

-- Loads the crossword puzzle from a file.
-- It reads the file, parses the crossword, and returns the resulting map of coordinates and characters.
-- >>> loadInput "example.txt"
-- fromList [((0,0),'M'),((0,1),'M'),((0,2),'S'),((0,3),'S'),((0,4),'X'),((0,5),'X'),((0,6),'M'),((0,7),'A'),((0,8),'M'),((0,9),'M'),((1,0),'X'),((1,1),'A'),((1,2),'A'),((1,3),'M'),((1,4),'X'),((1,5),'M'),((1,6),'S'),((1,7),'M'),((1,8),'S'),((1,9),'M'),((2,0),'M'),((2,1),'M'),((2,2),'X'),((2,3),'S'),((2,4),'A'),((2,5),'A'),((2,6),'A'),((2,7),'X'),((2,8),'A'),((2,9),'M'),((3,0),'X'),((3,1),'M'),((3,2),'A'),((3,3),'M'),((3,4),'M'),((3,5),'S'),((3,6),'M'),((3,7),'S'),((3,8),'M'),((3,9),'S'),((4,0),'A'),((4,1),'M'),((4,2),'M'),((4,3),'S'),((4,4),'M'),((4,5),'A'),((4,6),'A'),((4,7),'X'),((4,8),'X'),((4,9),'X'),((5,0),'X'),((5,1),'X'),((5,2),'A'),((5,3),'A'),((5,4),'X'),((5,5),'M'),((5,6),'S'),((5,7),'M'),((5,8),'M'),((5,9),'X'),((6,0),'M'),((6,1),'M'),((6,2),'S'),((6,3),'S'),((6,4),'X'),((6,5),'X'),((6,6),'M'),((6,7),'A'),((6,8),'S'),((6,9),'M'),((7,0),'A'),((7,1),'M'),((7,2),'A'),((7,3),'X'),((7,4),'A'),((7,5),'A'),((7,6),'S'),((7,7),'A'),((7,8),'M'),((7,9),'A'),((8,0),'S'),((8,1),'M'),((8,2),'A'),((8,3),'S'),((8,4),'M'),((8,5),'M'),((8,6),'M'),((8,7),'M'),((8,8),'S'),((8,9),'S'),((9,0),'X'),((9,1),'M'),((9,2),'A'),((9,3),'S'),((9,4),'A'),((9,5),'M'),((9,6),'X'),((9,7),'M'),((9,8),'A'),((9,9),'M')]
loadInput :: [Char] -> IO Crossword
loadInput = (fromRight M.empty . A.parseOnly parseCrossword <$>) . BSC.readFile . ("src/" ++)