{-# LANGUAGE OverloadedStrings, Strict #-}

module Lib
  ( loadInput
  , step
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

matchingTiles :: RemainingTiles -> Int -> [(Tile, Orientation)]
matchingTiles (tilesMap, tilesForEdge) value =
  case IM.lookup value tilesForEdge of
    Nothing -> []
    Just tileIds ->
      map
          ( (\tile@(Tile (_, edges, _)) ->
              (tile, snd . fromJust $ find ((== value) . fst) edges)
            )
          . (tilesMap IM.!)
          )
        $ S.toList tileIds

step :: State -> State
step (boardState, remainingTiles@(tiles, tilesForEdge))
  | any null matches = trace "no matches, rotate"
                             (rotatedBoardState, remainingTiles)
  | otherwise = trace ("matches " ++ show matches) (boardState, remainingTiles)
 where
  rotatedBoardState =
    M.map
        (\(tile, (rotation, flipstate)) ->
          (tile, (rotate90 rotation, flipstate))
        )
      $ M.mapKeys (\(x, y) -> (y, -x)) boardState
  matches      = map (matchingTiles remainingTiles) bottomValues
  yMax         = maximum $ map snd $ M.keys boardState
  bottomValues = map getBottomEdgeValue bottomEdge
  bottomEdge =
    map (boardState M.!) $ filter ((== yMax) . snd) $ M.keys boardState

