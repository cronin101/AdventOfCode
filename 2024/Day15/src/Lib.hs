{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second), first)
import Data
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Set qualified as S

parseTile :: A.Parser Tile
parseTile =
  A.choice
    [ Wall <$ "#",
      Box <$ "O",
      Robot <$ "@"
    ]

parseDirection :: A.Parser Direction
parseDirection =
  A.choice
    [ U <$ "^",
      D <$ "v",
      L <$ "<",
      R <$ ">"
    ]

-- >>> A.parseOnly parseRow "#..O..O.O#"
-- Right [((0,0),#),((3,0),O),((6,0),O),((8,0),O),((9,0),#)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = map (second fromJust) . filter (isJust . snd) . zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (Just <$> parseTile <|> Nothing <$ ".")

parseRoom :: A.Parser Room
parseRoom = Room . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)

parseState :: A.Parser State
parseState = do
  room <- parseRoom <* A.count 2 A.endOfLine
  directions <- catMaybes <$> A.many1 (Just <$> parseDirection <|> Nothing <$ A.endOfLine)
  return $ State room (head . M.keys . M.filter (== Robot) $ tiles' room) directions

toVector :: Direction -> Coord2D
toVector U = (0, 1)
toVector D = (0, -1)
toVector L = (-1, 0)
toVector R = (1, 0)

add :: Coord2D -> Coord2D -> Coord2D
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextCollisions :: Room -> Direction -> (Coord2D, Tile) -> S.Set (Coord2D, Tile)
nextCollisions (Room tiles) direction (actor, _) =
  let next = add (toVector direction) actor
      vertical = direction == U || direction == D
   in case (next, M.lookup (add (toVector direction) actor) tiles) of
        (c@(x, y), Just LeftBox)
          | vertical -> S.fromList [((x, y), LeftBox), ((x + 1, y), RightBox)]
          | otherwise -> S.singleton (c, LeftBox)
        (c@(x, y), Just RightBox)
          | vertical -> S.fromList [((x - 1, y), LeftBox), ((x, y), RightBox)]
          | otherwise -> S.singleton (c, RightBox)
        (c, Just t) -> S.singleton (c, t)
        _ -> S.empty

allCollisions :: Room -> Direction -> Coord2D -> S.Set (Coord2D, Tile)
allCollisions room direction robot =
  let propagate = S.unions . S.map (nextCollisions room direction)
   in S.unions $ takeWhile (not . S.null) $ iterate propagate (S.singleton (robot, Robot))

step :: State -> State
step s@(State _ _ []) = s
step s@(State room@(Room tiles) robot (d : ds))
  | S.member Wall $ S.map snd collisions = s {directions' = ds}
  | otherwise = s {room' = room'', robot' = robot'', directions' = ds}
  where
    move = add (toVector d)
    robot'' = move robot
    collisions = allCollisions room d robot
    unmoved = tiles M.\\ M.fromList (S.toList collisions)
    moved = M.fromList (map (first move) $ S.toList collisions)
    room'' = Room $ M.union moved unmoved

solve :: State -> State
solve = until (null . directions') step

gpsCoordinate :: Room -> Coord2D -> Int
gpsCoordinate (Room tiles) (x, y) = (x - minX) + (100 * (maxY - y))
  where
    coords = M.keys tiles
    minX = minimum . map fst $ coords
    maxY = maximum . map snd $ coords

coordinateSum :: Room -> Int
coordinateSum r = sum $ map (gpsCoordinate r) $ M.keys $ M.filter (\case Box -> True; LeftBox -> True; _ -> False) $ tiles' r

-- >>> solve <$> loadInput "example.txt"
-- State {room' =
-- ##########
-- #.O.O.OOO#
-- #........#
-- #OO......#
-- #OO@.....#
-- #O#.....O#
-- #O.....OO#
-- #O.....OO#
-- #OO....OO#
-- ##########, robot' = (3,5), directions' = []}

-- >>> part1 <$> loadInput "example.txt"
-- 10092
part1 :: State -> Int
part1 = coordinateSum . room' . solve

-- >>> loadInput "example.txt"
-- State {room' =
-- ##########
-- #..O..O.O#
-- #......O.#
-- #.OO..O.O#
-- #..O@..O.#
-- #O#..O...#
-- #O..O..O.#
-- #.OO.O.OO#
-- #....O...#
-- ##########, robot' = (4,5), directions' = [<,v,v,>,^,<,v,^,>,v,>,^,v,v,^,v,>,v,<,>,v,^,v,<,v,<,^,v,v,<,<,<,^,>,<,<,>,<,>,>,v,<,v,v,v,<,>,^,v,^,>,^,<,<,<,>,<,<,v,<,<,<,v,^,v,v,^,v,>,^,v,v,v,<,<,^,>,^,v,^,^,>,<,<,>,>,>,<,>,^,<,<,>,<,^,v,v,^,^,<,>,v,v,v,<,>,>,<,^,^,v,>,^,>,v,v,<,>,v,<,<,<,<,v,<,^,v,>,^,<,^,^,>,>,>,^,<,v,<,v,>,<,>,v,v,>,v,^,v,^,<,>,>,<,>,>,>,>,<,^,^,>,v,v,>,v,<,^,^,^,>,>,v,^,v,^,<,^,^,>,v,^,^,>,v,^,<,^,v,>,v,<,>,>,v,^,v,^,<,v,>,v,^,^,<,^,^,v,v,<,<,<,v,<,^,>,>,^,^,^,^,>,>,>,v,^,<,>,v,v,v,^,>,<,v,<,<,<,>,^,^,^,v,v,^,<,v,v,v,>,^,>,v,<,^,^,^,^,v,<,>,^,>,v,v,v,v,>,<,>,>,v,^,<,<,^,^,^,^,^,^,>,<,^,>,<,>,>,>,<,>,^,^,<,<,^,^,v,>,>,>,<,^,<,v,>,^,<,v,v,>,>,v,>,>,>,^,v,>,<,>,^,v,>,<,<,<,<,v,>,>,v,<,v,<,v,>,v,v,v,>,^,<,>,<,<,>,^,>,<,^,>,>,<,>,^,v,<,>,<,^,v,v,v,<,^,^,<,>,<,v,<,<,<,<,<,>,<,^,v,<,<,<,>,<,<,<,^,^,<,v,<,^,^,^,>,<,^,>,>,^,<,v,^,>,<,<,<,^,>,>,^,v,<,v,^,v,<,v,^,>,^,>,>,^,v,>,v,v,>,^,<,<,^,v,<,>,>,<,<,>,<,<,v,<,<,v,>,<,>,v,<,^,v,v,<,<,<,>,^,^,v,^,>,^,^,>,>,>,<,<,^,v,>,>,v,^,v,>,<,^,^,>,>,^,<,>,v,v,^,<,>,<,^,^,>,^,^,^,<,>,<,v,v,v,v,v,^,v,<,v,<,<,>,^,v,<,v,>,v,<,<,^,>,<,<,>,<,<,>,<,<,<,^,^,<,<,<,^,<,<,>,>,<,<,>,<,^,^,^,>,^,^,<,>,^,>,v,<,>,^,^,>,v,v,<,^,v,^,v,<,v,v,>,^,<,>,<,v,<,^,v,>,^,^,^,>,>,>,^,^,v,v,v,^,>,v,v,v,<,>,>,>,^,<,^,>,>,>,>,>,^,<,<,^,v,>,^,v,v,v,<,>,^,<,>,<,<,v,>,v,^,^,>,>,>,<,<,^,^,<,>,>,^,v,^,<,v,^,v,v,<,>,v,^,<,<,>,^,<,^,v,^,v,>,<,^,<,<,<,>,<,<,^,<,v,>,<,v,<,>,v,v,>,>,v,>,<,v,^,<,v,v,<,>,v,^,<,<,^]}
loadInput :: [Char] -> IO State
loadInput = (fromRight (State (Room M.empty) (0, 0) []) . A.parseOnly parseState <$>) . BSC.readFile . ("src/" ++)

-- >>>  expand <$> loadInput "example.txt"
-- State {room' =
-- ####################
-- ##....[]....[]..[]##
-- ##............[]..##
-- ##..[][]....[]..[]##
-- ##....[]@.....[]..##
-- ##[]##....[]......##
-- ##[]....[]....[]..##
-- ##..[][]..[]..[][]##
-- ##........[]......##
-- ####################, robot' = (7,5), directions' = [<,v,v,>,^,<,v,^,>,v,>,^,v,v,^,v,>,v,<,>,v,^,v,<,v,<,^,v,v,<,<,<,^,>,<,<,>,<,>,>,v,<,v,v,v,<,>,^,v,^,>,^,<,<,<,>,<,<,v,<,<,<,v,^,v,v,^,v,>,^,v,v,v,<,<,^,>,^,v,^,^,>,<,<,>,>,>,<,>,^,<,<,>,<,^,v,v,^,^,<,>,v,v,v,<,>,>,<,^,^,v,>,^,>,v,v,<,>,v,<,<,<,<,v,<,^,v,>,^,<,^,^,>,>,>,^,<,v,<,v,>,<,>,v,v,>,v,^,v,^,<,>,>,<,>,>,>,>,<,^,^,>,v,v,>,v,<,^,^,^,>,>,v,^,v,^,<,^,^,>,v,^,^,>,v,^,<,^,v,>,v,<,>,>,v,^,v,^,<,v,>,v,^,^,<,^,^,v,v,<,<,<,v,<,^,>,>,^,^,^,^,>,>,>,v,^,<,>,v,v,v,^,>,<,v,<,<,<,>,^,^,^,v,v,^,<,v,v,v,>,^,>,v,<,^,^,^,^,v,<,>,^,>,v,v,v,v,>,<,>,>,v,^,<,<,^,^,^,^,^,^,>,<,^,>,<,>,>,>,<,>,^,^,<,<,^,^,v,>,>,>,<,^,<,v,>,^,<,v,v,>,>,v,>,>,>,^,v,>,<,>,^,v,>,<,<,<,<,v,>,>,v,<,v,<,v,>,v,v,v,>,^,<,>,<,<,>,^,>,<,^,>,>,<,>,^,v,<,>,<,^,v,v,v,<,^,^,<,>,<,v,<,<,<,<,<,>,<,^,v,<,<,<,>,<,<,<,^,^,<,v,<,^,^,^,>,<,^,>,>,^,<,v,^,>,<,<,<,^,>,>,^,v,<,v,^,v,<,v,^,>,^,>,>,^,v,>,v,v,>,^,<,<,^,v,<,>,>,<,<,>,<,<,v,<,<,v,>,<,>,v,<,^,v,v,<,<,<,>,^,^,v,^,>,^,^,>,>,>,<,<,^,v,>,>,v,^,v,>,<,^,^,>,>,^,<,>,v,v,^,<,>,<,^,^,>,^,^,^,<,>,<,v,v,v,v,v,^,v,<,v,<,<,>,^,v,<,v,>,v,<,<,^,>,<,<,>,<,<,>,<,<,<,^,^,<,<,<,^,<,<,>,>,<,<,>,<,^,^,^,>,^,^,<,>,^,>,v,<,>,^,^,>,v,v,<,^,v,^,v,<,v,v,>,^,<,>,<,v,<,^,v,>,^,^,^,>,>,>,^,^,v,v,v,^,>,v,v,v,<,>,>,>,^,<,^,>,>,>,>,>,^,<,<,^,v,>,^,v,v,v,<,>,^,<,>,<,<,v,>,v,^,^,>,>,>,<,<,^,^,<,>,>,^,v,^,<,v,^,v,v,<,>,v,^,<,<,>,^,<,^,v,^,v,>,<,^,<,<,<,>,<,<,^,<,v,>,<,v,<,>,v,v,>,>,v,>,<,v,^,<,v,v,<,>,v,^,<,<,^]}
expand :: State -> State
expand (State (Room tiles) robot d) = State (Room tiles'') (shift robot) d
  where
    shift (x, y) = ((2 * x) - 1, y)
    tiles'' =
      M.fromList $
        concatMap (\case (c@(x, y), Box) -> [(shift c, LeftBox), ((2 * x, y), RightBox)]; (c@(x, y), Wall) -> [(shift c, Wall), ((2 * x, y), Wall)]; (c, t) -> [(shift c, t)]) $
          M.toList tiles

-- >>> solve . expand <$> loadInput "example.txt"
-- State {room' =
-- ####################
-- ##[].......[].[][]##
-- ##[]...........[].##
-- ##[]........[][][]##
-- ##[]......[]....[]##
-- ##..##......[]....##
-- ##..[]............##
-- ##..@......[].[][]##
-- ##......[][]..[]..##
-- ####################, robot' = (3,2), directions' = []}

-- >>> part2 <$> loadInput "example.txt"
-- 9021
part2 :: State -> Int
part2 = coordinateSum . room' . solve . expand
