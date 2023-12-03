{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

data Entity = Number {nOrigin :: Coord2D, nValue :: Int, nDigits :: Int} | Symbol {sOrigin :: Coord2D, sValue :: Char} deriving (Show, Eq, Ord)

type Coord2D = (Int, Int)

type Schematic = M.Map Coord2D Entity

-- >>> A.parseOnly parseNumber "123"
-- Right (Number {nOrigin = (0,0), nValue = 123, nDigits = 3})
parseNumber :: A.Parser Entity
parseNumber = do
  digits <- A.many1 A.digit
  return $ Number (0, 0) (read digits) (length digits)

parseEntity :: A.Parser Entity
parseEntity = parseNumber <|> (Symbol (0, 0) <$> A.notChar '\n')

-- >>> A.parseOnly parseLine "467..114.."
-- Right [Just (Number {nOrigin = (0,0), nValue = 467, nDigits = 3}),Nothing,Nothing,Just (Number {nOrigin = (0,0), nValue = 114, nDigits = 3}),Nothing,Nothing]
parseLine :: A.Parser [Maybe Entity]
parseLine = A.many1 ("." $> Nothing <|> (Just <$> parseEntity))

-- >>> addXCoords 0 0 [] [Just (Number (0, 0) 467 3), Nothing, Nothing, Just (Number (0, 0) 114 3), Nothing, Nothing]
-- [((0,0),Number {nOrigin = (0,0), nValue = 467, nDigits = 3}),((5,0),Number {nOrigin = (5,0), nValue = 114, nDigits = 3})]
addXCoords :: Int -> Int -> [(Coord2D, Entity)] -> [Maybe Entity] -> [(Coord2D, Entity)]
addXCoords _ _ done [] = done
addXCoords x y done (Nothing : rest) = addXCoords (x + 1) y done rest
addXCoords x y done (Just (Number _ val digits) : rest) = addXCoords (x + digits) y (done ++ [((x, y), Number (x, y) val digits)]) rest
addXCoords x y done (Just (Symbol _ value) : rest) = addXCoords (x + 1) y (done ++ [((x, y), Symbol (x, y) value)]) rest

-- Turns the 2D Array into a Map, adding the coordinates to the entities
addCoords :: [[Maybe Entity]] -> Schematic
addCoords schematic = M.fromList $ concat $ zipWith (\line y -> addXCoords 0 y [] line) schematic [0 ..]

parseSchematic :: A.Parser Schematic
parseSchematic = addCoords <$> A.sepBy1 parseLine A.endOfLine

-- >>> loadInput "example.txt"
-- fromList [((0,0),Number {nOrigin = (0,0), nValue = 467, nDigits = 3}),((0,4),Number {nOrigin = (0,4), nValue = 617, nDigits = 3}),((1,9),Number {nOrigin = (1,9), nValue = 664, nDigits = 3}),((2,2),Number {nOrigin = (2,2), nValue = 35, nDigits = 2}),((2,6),Number {nOrigin = (2,6), nValue = 592, nDigits = 3}),((3,1),Symbol {sOrigins = (3,1), sValue = '*'}),((3,4),Symbol {sOrigins = (3,4), sValue = '*'}),((3,8),Symbol {sOrigins = (3,8), sValue = '$'}),((5,0),Number {nOrigin = (5,0), nValue = 114, nDigits = 3}),((5,5),Symbol {sOrigins = (5,5), sValue = '+'}),((5,8),Symbol {sOrigins = (5,8), sValue = '*'}),((5,9),Number {nOrigin = (5,9), nValue = 598, nDigits = 3}),((6,2),Number {nOrigin = (6,2), nValue = 633, nDigits = 3}),((6,3),Symbol {sOrigins = (6,3), sValue = '#'}),((6,7),Number {nOrigin = (6,7), nValue = 755, nDigits = 3}),((7,5),Number {nOrigin = (7,5), nValue = 58, nDigits = 2})]
loadInput :: [Char] -> IO Schematic
loadInput = (fromRight M.empty . A.parseOnly parseSchematic <$>) . BSC.readFile . ("src/" ++)

-- >>> adjacents (Number (0, 0) 467 3)
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(0,-1),(1,-1),(2,-1),(0,1),(1,1),(2,1),(2,-1),(2,1),(3,-1),(3,0),(3,1)]
adjacents :: Entity -> [Coord2D]
adjacents (Symbol (x, y) _) = filter (/= (x, y)) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
adjacents (Number (x, y) _ digits) =
  (filter (/= (x, y)) [(x + dx, y + dy) | dx <- [-1, 0], dy <- [-1, 0, 1]])
    ++ [(x', y + dy) | dy <- [-1, 1], x' <- [x .. x + digits - 1]]
    ++ filter (/= (x + digits - 1, y)) [(x + (digits - 1) + dx, y + dy) | dx <- [0, 1], dy <- [-1, 0, 1]]

-- >>> part1 <$> loadInput "example.txt"
-- 4361
part1 :: Schematic -> Int
part1 schematic = sum $ map nValue parts
  where
    isJustSymbol (Just (Symbol {})) = True
    isJustSymbol _ = False
    parts = filter hasAdjacentSymbol numbers
    hasAdjacentSymbol = any (\adjCoord -> isJustSymbol (adjCoord `M.lookup` schematic)) . adjacents
    numbers = filter (\case (Number {}) -> True; _ -> False) $ M.elems schematic

-- >>> part2 <$> loadInput "example.txt"
-- 467835
part2 :: Schematic -> Int
part2 schematic = sum $ map (product . map nValue . adjacentNumbers) gearsWithTwoAdjacentNumbers
  where
    gearsWithTwoAdjacentNumbers = filter ((== 2) . length . adjacentNumbers) gears
    adjacentNumbers = S.toList . S.fromList . mapMaybe (`M.lookup` expandedNumbers) . adjacents
    -- Schematic with "duplicated" numbers, so that we can easily find the adjacent numbers of a gear
    expandedNumbers = M.fromList $ concatMap duplicateNumber $ filter (\case (Number {}) -> True; _ -> False) $ M.elems schematic
    duplicateNumber n@(Number (x, y) _ digits) = [((x + dx, y), n) | dx <- [0 .. digits - 1]]
    gears = filter (\case (Symbol _ '*') -> True; _ -> False) $ M.elems schematic
