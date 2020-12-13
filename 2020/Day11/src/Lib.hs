{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , stableState
  , passengerCount
  , nextState
  , PassengerTolerance(PassengerTolerance)
  , AdjacencyType(Immediate, NearestVisible)
  ) where

import           Data.Map.Strict                ( Map )
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.Map.Strict               as M
import qualified Data.IntMap.Strict            as IM
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BSC
import qualified Data.IntSet                   as S
import           Data.IntSet                    ( IntSet )
import           Data.List                      ( intercalate
                                                , foldl'
                                                , find
                                                , iterate'
                                                )
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( mapMaybe )

data SeatStatus = Free | Taken
    deriving (Show, Eq)

data AdjacencyType = Immediate | NearestVisible
    deriving (Show, Eq)

data PassengerTolerance = PassengerTolerance AdjacencyType Int
  deriving (Show, Eq)

data Ferry = Ferry

  { seats                   :: Map (Int, Int) SeatStatus
  , adjacentPassengerCount  :: IntMap Int
  , activeSeats             :: IntSet
  , adjacentSeatsMap        :: IntMap IntSet
  , dimensions              :: (Int, Int)
  , neighbourToleranceLimit :: Int
  }
instance Show Ferry  where
  show f = intercalate
    "\n"
    [ [ showSeat (x, y) | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]
   where
    (width, height) = dimensions f
    showSeat (x, y) = case M.lookup (x, y) (seats f) of
      (Just Free ) -> 'L'
      (Just Taken) -> '#'
      Nothing      -> '.'

seatsInRow :: Int -> ByteString -> Map (Int, Int) SeatStatus
seatsInRow y bytestring =
  M.fromList [ ((x, y), Free) | x <- BSC.elemIndices 'L' bytestring ]

toSeatId :: (Int, Int) -> Int -> Int
toSeatId (x, y) width = x + (width * y)

fromSeatId :: Int -> Int -> (Int, Int)
fromSeatId id width = swap $ id `quotRem` width

adjacentCoordinates :: (Int, Int) -> [(Int, Int)]
adjacentCoordinates (x, y) =
  [ (x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1], (x', y') /= (0, 0) ]

visibleCoordinates
  :: (Int, Int) -> (Int, Int) -> Map (Int, Int) SeatStatus -> [(Int, Int)]
visibleCoordinates seat (width, height) seats = mapMaybe
  (\(x'', y'') ->
    find (`M.member` seats)
      $ takeWhile (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height)
      $ drop 1
      $ iterate (\(x', y') -> (x' + x'', y' + y'')) seat
  )
  vectors
  where vectors = adjacentCoordinates (0, 0)

updateSeatState :: Ferry -> (Int, Int) -> Ferry
updateSeatState (Ferry seats previousAdjacentPassengerCount activeSeats adjacentSeatsMap dimensions@(width, _) tolerance) seat
  = Ferry seats'
          previousAdjacentPassengerCount
          activeSeats'
          adjacentSeatsMap
          dimensions
          tolerance
 where
  activeSeats' = if seatChanged
    then S.union activeSeats (adjacentSeatsMap IM.! toSeatId seat width)
    else activeSeats
  seats'      = if seatChanged then M.insert seat nextSeatState seats else seats
  seatChanged = nextSeatState /= previousSeatState
  previousAdjacentCount =
    IM.findWithDefault 0 (toSeatId seat width) previousAdjacentPassengerCount
  previousSeatState = seats M.! seat
  nextSeatState | previousAdjacentCount == 0        = Taken
                | previousAdjacentCount > tolerance = Free
                | otherwise                         = previousSeatState

updateAdjacentPassengers :: Ferry -> Ferry
updateAdjacentPassengers (Ferry seats adjacentPassengerCount activeSeats adjacentSeatsMap dimensions@(width, _) tolerance)
  = Ferry seats
          adjacentPassengerCount'
          activeSeats
          adjacentSeatsMap
          dimensions
          tolerance
 where
  adjacentPassengerCount' = IM.union
    (IM.fromList
      [ ( toSeatId seat width
        , S.size
        $ S.filter ((== Just Taken) . (`M.lookup` seats) . (`fromSeatId` width))
        $ adjacentSeatsMap
        IM.! toSeatId seat width
        )
      | seat <- map (`fromSeatId` width) $ S.toAscList activeSeats
      ]
    )
    adjacentPassengerCount

nextState :: Ferry -> Ferry
nextState (Ferry seats adjacentPassengerCount activeSeats adjacentSeatsMap dimensions@(width, _) tolerance)
  = updateAdjacentPassengers
    . foldl'
        updateSeatState
        (Ferry seats
               adjacentPassengerCount
               S.empty
               adjacentSeatsMap
               dimensions
               tolerance
        )
    $ map (`fromSeatId` width)
    $ S.toList activeSeats

stableState :: Ferry -> Ferry
stableState ferry =
  head . dropWhile (not . S.null . activeSeats) $ drop 1 $ iterate' nextState
                                                                    ferry

passengerCount :: Ferry -> Int
passengerCount (Ferry seats _ _ _ _ _) =
  length $ filter ((== Taken) . snd) $ M.toList seats

loadRows :: [ByteString] -> PassengerTolerance -> Ferry
loadRows rows@(firstRow : _) (PassengerTolerance adjacencyType tolerance) =
  Ferry seats
        (IM.fromList $ [ (toSeatId seat width, 0) | seat <- M.keys seats ])
        (S.fromList $ map (`toSeatId` width) $ M.keys seats)
        adjacents
        (width, height)
        tolerance
 where
  seats =
    M.unions [ seatsInRow y bytestring | (y, bytestring) <- zip [0 ..] rows ]
  adjacents = IM.fromList
    [ ( toSeatId seat width
      , S.fromList
      $ map (`toSeatId` width)
      $ filter (`M.member` seats)
      $ (case adjacencyType of
          Immediate -> adjacentCoordinates
          NearestVisible ->
            (\seat -> visibleCoordinates seat (width, height) seats)
        )
          seat
      )
    | seat <- M.keys seats
    ]
  width  = BSC.length firstRow
  height = length rows

loadInput :: String -> PassengerTolerance -> IO Ferry
loadInput fileName params =
  (`loadRows` params) . BSC.lines <$> BSC.readFile ("src/" ++ fileName)
