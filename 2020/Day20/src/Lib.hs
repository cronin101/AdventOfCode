{-# LANGUAGE OverloadedStrings, Strict #-}

module Lib
  ( loadInput
  , step
  , solve
  , multiplyCorners
  ) where

import qualified Data.ByteString.Char8         as BS
import           Data.Attoparsec.ByteString.Char8
                                                ( parseOnly
                                                , sepBy1
                                                , isSpace
                                                , endOfLine
                                                , many1
                                                , isDigit
                                                , takeWhile1
                                                , string
                                                , Parser
                                                )
import qualified Data.Map.Strict               as M
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as S
import           Debug.Trace                    ( trace )
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.List                      ( find )

-- Unflipped | Flipped
data FlipS = U  | F
 deriving(Show, Enum, Eq)

-- Clockwise Rotation: 0deg, 90deg, 180deg, 270deg
data RotateS = CW0 | CW90 | CW180 | CW270
 deriving(Show, Enum, Eq, Ord)

type Orientation = (RotateS, FlipS)

type Value = Int
type TopEdge = (Value, Orientation)

type TileId = Int

--Bitmap stores ByteString Rows
newtype Bitmap = Bitmap [BS.ByteString]

instance Show Bitmap where
  show (Bitmap rows) = BS.unpack $ BS.concat $ map (`BS.snoc` '\n') rows

newtype Tile = Tile (TileId, [TopEdge], Bitmap)

-- Maps an Edge Value to a Set of possible tiles that can be placed there
type TilesForEdge = IM.IntMap S.IntSet

type Tiles = IM.IntMap Tile

type RemainingTiles = (Tiles, TilesForEdge)

instance Show Tile where
  show (Tile (id, edges, bitmap)) =
    "(Tile "
      ++ show id
      ++ ":\nEdges: "
      ++ show edges
      ++ "\n"
      ++ show bitmap
      ++ ")"

type BoardState = (M.Map (Int, Int) (Tile, Orientation))

type State = (BoardState, RemainingTiles)

rotate90 :: RotateS -> RotateS
rotate90 start = dropWhile (/= start) (cycle [CW0 .. CW270]) !! 1

parseBitmap :: Parser Bitmap
parseBitmap = do
  rows <- many1 $ takeWhile1 (not . isSpace) <* endOfLine
  return $ Bitmap rows

toEdgeValue :: BS.ByteString -> Orientation -> [TopEdge]
toEdgeValue bs orientation@(rotation, flipState) =
  [(value indices, orientation), (value flippedIndices, (rotation, flipped))]
 where
  flipped        = if flipState == F then U else F
  value          = sum . map (2 ^)
  indices        = map ((bitCount -) . (1 +)) flippedIndices
  flippedIndices = BS.elemIndices '#' bs
  bitCount       = BS.length bs

getBottomEdgeValue :: (Tile, (RotateS, FlipS)) -> Value
getBottomEdgeValue (Tile (_, edges, _), (rotation, flipState)) =
  fst $ fromJust . find (rotationTest . fst . snd) $ filter
    (flipStateTest . snd . snd)
    edges
 where
  flipStateTest    = if rotation <= CW90 then (/= flipState) else (== flipState)
  rotationTest     = (== dersiredRotation)
  dersiredRotation = dropWhile (/= rotation) (cycle [CW0 .. CW270]) !! 2

parseTile :: Parser Tile
parseTile = do
  Just (id, _) <-
    string "Tile "
    *> (BS.readInt <$> takeWhile1 isDigit)
    <* string ":"
    <* endOfLine
  bitmap@(Bitmap rows) <- parseBitmap
  let topEdge    = (head rows, (CW0, U))
  let rightEdge  = (BS.pack $ map BS.last rows, (CW90, U))
  let bottomEdge = (last rows, (CW180, F))
  let leftEdge   = (BS.pack $ map BS.head rows, (CW270, F))
  return $ Tile
    ( id
    , concatMap (uncurry toEdgeValue) [topEdge, rightEdge, bottomEdge, leftEdge]
    , bitmap
    )

parseTiles :: Parser [Tile]
parseTiles = sepBy1 parseTile endOfLine

loadInput :: [Char] -> IO State
loadInput fileName = do
  Right (startTile : tiles) <- parseOnly parseTiles
    <$> BS.readFile ("src/" ++ fileName)
  let tileMap = IM.fromList $ map (\tile@(Tile (id, _, _)) -> (id, tile)) tiles
  let edgesWithTiles = concatMap
        (\(Tile (id, edges, _)) -> zip (map fst edges) (repeat id))
        tiles
  let edgeMap = foldl
        (\m (value, tileId) ->
          IM.insertWith S.union value (S.singleton tileId) m
        )
        IM.empty
        edgesWithTiles
  return (M.singleton (0, 0) (startTile, (CW0, U)), (tileMap, edgeMap))

matchingTilesForValue :: RemainingTiles -> Int -> [(Tile, Orientation)]
matchingTilesForValue (tilesMap, tilesForEdge) value =
  case IM.lookup value tilesForEdge of
    Nothing -> []
    Just tileIds ->
      map
          (\tile@(Tile (_, edges, _)) ->
            (tile, snd . fromJust $ find ((== value) . fst) edges)
          )
        $ mapMaybe (`IM.lookup` tilesMap)
        $ S.toList tileIds

matchingTilesForBottomEdge :: State -> [[(Tile, Orientation)]]
matchingTilesForBottomEdge (boardState, remainingTiles) = map
  (matchingTilesForValue remainingTiles)
  bottomValues
  where bottomValues = map (getBottomEdgeValue . snd) $ bottomEdge boardState


bottomEdge :: BoardState -> [((Int, Int), (Tile, Orientation))]
bottomEdge boardState =
  map (\coord -> (coord, boardState M.! coord))
    $ filter ((== yMax) . snd)
    $ M.keys boardState
  where yMax = maximum $ map snd $ M.keys boardState

insertMatchesIntoBoardState
  :: BoardState -> [((Int, Int), (Tile, Orientation))] -> BoardState
insertMatchesIntoBoardState = foldl (\bs ((x, y), t) -> M.insert (x, y) t bs)

removeMatchesFromRemainingTiles :: RemainingTiles -> [Tile] -> RemainingTiles
removeMatchesFromRemainingTiles (tiles, tilesForEdge) matches =
  (tiles', tilesForEdge)
  where tiles' = foldl (\t (Tile (id, _, _)) -> IM.delete id t) tiles matches

rotateBoardState :: BoardState -> BoardState
rotateBoardState boardState =
  M.map
      (\(tile, (rotation, flipstate)) -> (tile, (rotate90 rotation, flipstate)))
    $ M.mapKeys (\(x, y) -> (y, -x)) boardState

step :: State -> State
step state@(boardState, remainingTiles@(tiles, tilesForEdge))
  | IM.size tiles == 0 = trace "No tiles remaining!" state
  | any null matches = trace "no matches, rotate"
                             (rotateBoardState boardState, remainingTiles)
  | all ((== 1) . length) matches = trace
    "trivial matches!"
    ( insertMatchesIntoBoardState boardState
                                  (zipMatchesWithCoords (concat matches))
    , removeMatchesFromRemainingTiles remainingTiles (map fst $ concat matches)
    )
  | otherwise = trace ("matches " ++ show matches) (boardState, remainingTiles)
 where
  matches = matchingTilesForBottomEdge state
  zipMatchesWithCoords prunedMatches = zip
    (map ((\(x, y) -> (x, y + 1)) . fst) $ bottomEdge boardState)
    prunedMatches


solve :: State -> State
solve state =
  head
    $ dropWhile (\(_, (tilesRemaining, _)) -> (not . IM.null) tilesRemaining)
    $ iterate step state

multiplyCorners :: BoardState -> Int
multiplyCorners boardState = product $ map
  ((\(Tile (id, _, _), _) -> id) . (boardState M.!))
  [(xLow, yLow), (xLow, yHigh), (xHigh, yLow), (xHigh, yHigh)]
 where
  xs          = map fst coordinates
  xLow        = minimum xs
  xHigh       = maximum xs
  ys          = map snd coordinates
  yLow        = minimum ys
  yHigh       = maximum ys
  coordinates = M.keys boardState


