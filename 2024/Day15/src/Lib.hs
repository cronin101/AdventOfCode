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

-- Parses a tile from the warehouse map.
parseTile :: A.Parser Tile
parseTile =
  A.choice
    [ Wall <$ "#",
      Box <$ "O",
      Robot <$ "@"
    ]

-- Parses a direction from the movement instructions.
parseDirection :: A.Parser Direction
parseDirection =
  A.choice
    [ U <$ "^",
      D <$ "v",
      L <$ "<",
      R <$ ">"
    ]

-- Parses a row of the warehouse map, identifying the coordinates of each tile.
-- >>> A.parseOnly parseRow "#..O..O.O#"
-- Right [((0,0),#),((3,0),O),((6,0),O),((8,0),O),((9,0),#)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = map (second fromJust) . filter (isJust . snd) . zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (Just <$> parseTile <|> Nothing <$ ".")

-- Parses the entire warehouse map, row by row.
parseRoom :: A.Parser Room
parseRoom = Room . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)

-- Parses the initial state by parsing the room and the list of directions.
parseState :: A.Parser State
parseState = do
  room <- parseRoom <* A.count 2 A.endOfLine
  directions <- catMaybes <$> A.many1 (Just <$> parseDirection <|> Nothing <$ A.endOfLine)
  return $ State room (head . M.keys . M.filter (== Robot) $ tiles' room) directions

-- Converts a direction into a vector for movement.
toVector :: Direction -> Coord2D
toVector U = (0, 1)
toVector D = (0, -1)
toVector L = (-1, 0)
toVector R = (1, 0)

-- Calculates the new position by adding vectors.
add :: Coord2D -> Coord2D -> Coord2D
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Predicts the next collisions based on the current direction and position.
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

-- Calculates all potential collisions for a given direction.
allCollisions :: Room -> Direction -> Coord2D -> S.Set (Coord2D, Tile)
allCollisions room direction robot =
  let propagate = S.unions . S.map (nextCollisions room direction)
   in S.unions $ takeWhile (not . S.null) $ iterate propagate (S.singleton (robot, Robot))

-- Executes a single step based on the current state and direction.
step :: State -> State
step s@(State _ _ []) = s -- No more directions
step s@(State room@(Room tiles) robot (d : ds))
  | S.member Wall $ S.map snd collisions = s {directions' = ds} -- Hit a wall
  | otherwise = s {room' = room'', robot' = robot'', directions' = ds}
  where
    move = add (toVector d)
    robot'' = move robot
    collisions = allCollisions room d robot -- All collisions in the direction of movement
    unmoved = tiles M.\\ M.fromList (S.toList collisions) -- Tiles that are not moved
    moved = M.fromList (map (first move) $ S.toList collisions) -- Tiles that are moved
    room'' = Room $ M.union moved unmoved

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
solve :: State -> State
solve = until (null . directions') step

-- Calculates the GPS coordinate of a box based on its position in the room.
gpsCoordinate :: Room -> Coord2D -> Int
gpsCoordinate (Room tiles) (x, y) = (x - minX) + (100 * (maxY - y))
  where
    coords = M.keys tiles
    minX = minimum . map fst $ coords
    maxY = maximum . map snd $ coords

-- Calculates the sum of all GPS coordinates for the boxes in the room.
coordinateSum :: Room -> Int
coordinateSum r = sum $ map (gpsCoordinate r) $ M.keys $ M.filter (\case Box -> True; LeftBox -> True; _ -> False) $ tiles' r

-- Solves the puzzle for part 1 by calculating the sum of GPS coordinates after executing all moves.
-- >>> part1 <$> loadInput "example.txt"
-- 10092
part1 :: State -> Int
part1 = coordinateSum . room' . solve

-- Loads the initial state from a file.
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

-- Expands the room to twice its width, adjusting the tiles accordingly.
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

-- Solves the puzzle for part 2 by expanding the room and then calculating the sum of GPS coordinates after executing all moves.
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
part2 :: State -> Int
part2 = coordinateSum . room' . solve . expand
