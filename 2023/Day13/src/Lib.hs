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
import Data.IntMap qualified as IM
import Data.List (find, transpose)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)

data Pattern = Pattern {rows :: IM.IntMap String, cols :: IM.IntMap String, rowCrossProductDifference :: M.Map (Int, Int) Int, columnCrossProductDifference :: M.Map (Int, Int) Int} deriving (Show, Eq, Ord)

-- >>> stringDifference "#.##..##." "..##..##."
-- 1
stringDifference :: String -> String -> Int
stringDifference s1 s2 = sum $ zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) s1 s2

parsePattern :: A.Parser Pattern
parsePattern = do
  rs <- A.many1 (A.choice [A.char '#', A.char '.']) `A.sepBy1` A.endOfLine
  let rowMap = IM.fromList $ zip [1 ..] rs
  let colMap = IM.fromList $ zip [1 ..] $ transpose rs
  return $ Pattern rowMap colMap (crossProductEquality rowMap) (crossProductEquality colMap)
  where
    crossProductEquality map = M.fromList [((r, r'), stringDifference (map IM.! r) (map IM.! r')) | r <- IM.keys map, r' <- IM.keys map, r < r']

parsePatterns :: A.Parser [Pattern]
parsePatterns = (parsePattern `A.sepBy1` A.count 2 A.endOfLine) <* A.endOfInput

mirror :: IM.IntMap String -> M.Map (Int, Int) Int -> Maybe Int
mirror entries crossProductDifference = find isMirror [1 .. IM.size entries - 1]
  where
    isMirror r = all (\i -> maybe True (== 0) (M.lookup (r + 1 - i, r + i) crossProductDifference)) [1 .. IM.size entries]

smudgedMirror :: IM.IntMap String -> M.Map (Int, Int) Int -> Maybe Int
smudgedMirror entries crossProductDifference = find isMirror [1 .. IM.size entries - 1]
  where
    isMirror r = case filter (/= 0) $ mapMaybe (\i -> M.lookup (r + 1 - i, r + i) crossProductDifference) [1 .. IM.size entries] of
      [1] -> True
      _ -> False

-- >>> map horizontalMirror <$> loadInput "example.txt"
-- [Nothing,Just 4]
horizontalMirror :: Pattern -> Maybe Int
horizontalMirror (Pattern rs _ rcpd _) = mirror rs rcpd

-- >>> map horizontalSmudgedMirror <$> loadInput "example.txt"
-- [Just 3,Just 1]
horizontalSmudgedMirror :: Pattern -> Maybe Int
horizontalSmudgedMirror (Pattern rs _ rcpd _) = smudgedMirror rs rcpd

-- >>> map verticalMirror <$> loadInput "example.txt"
-- [Just 5,Nothing]
verticalMirror :: Pattern -> Maybe Int
verticalMirror (Pattern _ cs _ ccpd) = mirror cs ccpd

-- >>> map verticalSmudgedMirror <$> loadInput "example.txt"
-- [Nothing,Nothing]
verticalSmudgedMirror :: Pattern -> Maybe Int
verticalSmudgedMirror (Pattern _ cs _ ccpd) = smudgedMirror cs ccpd

score :: (Pattern -> Maybe Int, Pattern -> Maybe Int) -> Pattern -> Int
score (verticalFinder, horizontalFinder) p = fromMaybe 0 (verticalFinder p) + maybe 0 (* 100) (horizontalFinder p)

-- >>> loadInput "example.txt"
-- [Pattern {rows = fromList [(1,"#.##..##."),(2,"..#.##.#."),(3,"##......#"),(4,"##......#"),(5,"..#.##.#."),(6,"..##..##."),(7,"#.#.##.#.")], cols = fromList [(1,"#.##..#"),(2,"..##..."),(3,"##..###"),(4,"#....#."),(5,".#..#.#"),(6,".#..#.#"),(7,"#....#."),(8,"##..###"),(9,"..##...")], rowCrossProductDifference = fromList [((1,2),5),((1,3),6),((1,4),6),((1,5),5),((1,6),1),((1,7),4),((2,3),7),((2,4),7),((2,5),0),((2,6),4),((2,7),1),((3,4),0),((3,5),7),((3,6),7),((3,7),6),((4,5),7),((4,6),7),((4,7),6),((5,6),4),((5,7),1),((6,7),5)], columnCrossProductDifference = fromList [((1,2),2),((1,3),5),((1,4),4),((1,5),5),((1,6),5),((1,7),4),((1,8),5),((1,9),2),((2,3),7),((2,4),4),((2,5),5),((2,6),5),((2,7),4),((2,8),7),((2,9),0),((3,4),3),((3,5),2),((3,6),2),((3,7),3),((3,8),0),((3,9),7),((4,5),5),((4,6),5),((4,7),0),((4,8),3),((4,9),4),((5,6),0),((5,7),5),((5,8),2),((5,9),5),((6,7),5),((6,8),2),((6,9),5),((7,8),3),((7,9),4),((8,9),7)]},Pattern {rows = fromList [(1,"#...##..#"),(2,"#....#..#"),(3,"..##..###"),(4,"#####.##."),(5,"#####.##."),(6,"..##..###"),(7,"#....#..#")], cols = fromList [(1,"##.##.#"),(2,"...##.."),(3,"..####."),(4,"..####."),(5,"#..##.."),(6,"##....#"),(7,"..####."),(8,"..####."),(9,"###..##")], rowCrossProductDifference = fromList [((1,2),1),((1,3),7),((1,4),7),((1,5),7),((1,6),7),((1,7),1),((2,3),6),((2,4),8),((2,5),8),((2,6),6),((2,7),0),((3,4),4),((3,5),4),((3,6),0),((3,7),6),((4,5),0),((4,6),4),((4,7),8),((5,6),4),((5,7),8),((6,7),6)], columnCrossProductDifference = fromList [((1,2),3),((1,3),5),((1,4),5),((1,5),2),((1,6),2),((1,7),5),((1,8),5),((1,9),4),((2,3),2),((2,4),2),((2,5),1),((2,6),5),((2,7),2),((2,8),2),((2,9),7),((3,4),0),((3,5),3),((3,6),7),((3,7),0),((3,8),0),((3,9),5),((4,5),3),((4,6),7),((4,7),0),((4,8),0),((4,9),5),((5,6),4),((5,7),3),((5,8),3),((5,9),6),((6,7),7),((6,8),7),((6,9),2),((7,8),0),((7,9),5),((8,9),5)]}]
loadInput :: [Char] -> IO [Pattern]
loadInput = (fromRight [] . A.parseOnly parsePatterns <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 405
part1 :: [Pattern] -> Int
part1 = sum . map (score (verticalMirror, horizontalMirror))

-- >>> part2 <$> loadInput "example.txt"
-- 400
part2 :: [Pattern] -> Int
part2 = sum . map (score (verticalSmudgedMirror, horizontalSmudgedMirror))
