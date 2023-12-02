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
import Data.Functor (($>))
import Data.Map qualified as M

data Ball = Red | Green | Blue deriving (Show, Eq, Ord)

type BallCount = M.Map Ball Int

data Game = Game {gameID :: Int, rounds :: [BallCount], upperBounds :: BallCount} deriving (Show)

type Input = [Game]

parseBall :: A.Parser Ball
parseBall = A.choice ["red" $> Red, "green" $> Green, "blue" $> Blue]

-- >>> A.parseOnly parseRound "3 green, 4 blue, 1 red"
-- Right (fromList [(Red,1),(Green,3),(Blue,4)])
parseRound :: A.Parser BallCount
parseRound = M.fromList <$> A.sepBy1 (flip (,) <$> (A.decimal <* " ") <*> parseBall) ", "

-- >>> A.parseOnly parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
-- Right (Game {gameID = 3, rounds = [fromList [(Red,20),(Green,8),(Blue,6)],fromList [(Red,4),(Green,13),(Blue,5)],fromList [(Red,1),(Green,5)]], upperBounds = fromList [(Red,20),(Green,13),(Blue,6)]})
parseGame :: A.Parser Game
parseGame = do
  gid <- "Game " *> A.decimal <* ": "
  r <- A.sepBy1 parseRound "; "
  let ub = M.unionsWith max r
  return $ Game gid r ub

-- >>> loadInput "example.txt"
-- [Game {gameID = 1, rounds = [fromList [(Red,4),(Blue,3)],fromList [(Red,1),(Green,2),(Blue,6)],fromList [(Green,2)]], upperBounds = fromList [(Red,4),(Green,2),(Blue,6)]},Game {gameID = 2, rounds = [fromList [(Green,2),(Blue,1)],fromList [(Red,1),(Green,3),(Blue,4)],fromList [(Green,1),(Blue,1)]], upperBounds = fromList [(Red,1),(Green,3),(Blue,4)]},Game {gameID = 3, rounds = [fromList [(Red,20),(Green,8),(Blue,6)],fromList [(Red,4),(Green,13),(Blue,5)],fromList [(Red,1),(Green,5)]], upperBounds = fromList [(Red,20),(Green,13),(Blue,6)]},Game {gameID = 4, rounds = [fromList [(Red,3),(Green,1),(Blue,6)],fromList [(Red,6),(Green,3)],fromList [(Red,14),(Green,3),(Blue,15)]], upperBounds = fromList [(Red,14),(Green,3),(Blue,15)]},Game {gameID = 5, rounds = [fromList [(Red,6),(Green,3),(Blue,1)],fromList [(Red,1),(Green,2),(Blue,2)]], upperBounds = fromList [(Red,6),(Green,3),(Blue,2)]}]
loadInput :: [Char] -> IO Input
loadInput = (fromRight [] . A.parseOnly (A.sepBy1 parseGame A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 8
part1 :: Input -> Int
part1 =
  let ballCount = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]
   in sum . map gameID . filter ((ballCount ==) . M.unionWith max ballCount . upperBounds)

-- >>> part2 <$> loadInput "example.txt"
-- 2286
part2 :: Input -> Int
part2 = sum . map (product . M.elems . upperBounds)
