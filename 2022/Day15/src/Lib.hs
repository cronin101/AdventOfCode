{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    showWorld,
    countNoBeacon,
    findBeacon,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (find, intercalate)
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing)

type Coordinate = (Int, Int)

type Bounds = (Coordinate, Coordinate)

data Entity
  = Sensor
      { location :: Coordinate,
        closestBeacon :: Coordinate,
        manhattanDistance :: Int
      }
  | Beacon Coordinate
  deriving (Show)

type World = (Bounds, M.Map Coordinate Entity)

-- >>> A.parseOnly  parseEntityPair  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
-- Right [Sensor {location = (2,18), closestBeacon = (-2,15), manhattanDistance = 7},Beacon (-2,15)]
parseEntityPair :: A.Parser (M.Map Coordinate Entity)
parseEntityPair = do
  sl@(x, y) <- (,) <$> ("Sensor at x=" *> A.signed A.decimal) <*> (", y=" *> A.signed A.decimal)
  cb@(x', y') <- (,) <$> (": closest beacon is at x=" *> A.signed A.decimal) <*> (", y=" *> A.signed A.decimal)
  let md = abs (x - x') + abs (y - y')
  return $ M.fromList [(sl, Sensor sl cb md), (cb, Beacon cb)]

parseEntities :: A.Parser (M.Map Coordinate Entity)
parseEntities = M.unions <$> A.sepBy1 parseEntityPair A.endOfLine

loadInput :: String -> IO World
loadInput = (withBounds . fromRight M.empty . A.parseOnly parseEntities <$>) . BSC.readFile . ("src/" ++)
  where
    withBounds :: M.Map Coordinate Entity -> World
    withBounds entities = (((minimum $ map fst minPoints, minimum $ map snd minPoints), (maximum $ map fst maxPoints, maximum $ map snd maxPoints)), entities)
      where
        maxPoints = map (\((x, y), d) -> (x + d, y + d)) sensorLocationsWithDistances
        minPoints = map (\((x, y), d) -> (x - d, y - d)) sensorLocationsWithDistances
        sensorLocationsWithDistances =
          map (\(Sensor l _ d) -> (l, d)) $ filter (\case Sensor {} -> True; _ -> False) $ M.elems entities

showWorld :: World -> String
showWorld w@(bounds@((xMin, yMin), (xMax, yMax)), m) =
  show bounds
    ++ "\n"
    ++ intercalate
      "\n"
      [ concat
          [ case M.lookup (x, y) m of
              Just (Sensor {}) -> "o"
              Just (Beacon {}) -> "●"
              _ -> if inRangeOfSensors w (x, y) then "." else " "
            | x <- [xMin .. xMax]
          ]
          ++ "Y="
          ++ show y
          ++ ": "
          ++ show (countNoBeacon w y)
        | y <- [yMin .. yMax]
      ]

sensorXSkipPoint :: World -> Coordinate -> Maybe Coordinate
sensorXSkipPoint w (x, y) = fmap ((,y) . onePastSensorXRange) maybeSensor
  where
    onePastSensorXRange (Sensor (x', y') _ md) = x' + (md - abs (y - y')) + 1
    maybeSensor = find (\case Sensor (x', y') _ md -> abs (x - x') + abs (y - y') <= md; _ -> False) $ M.elems $ snd w

inRangeOfSensors :: World -> Coordinate -> Bool
inRangeOfSensors w c = isJust $ sensorXSkipPoint w c

potentialBeaconPoints :: World -> Int -> (Int, Int) -> [Coordinate]
potentialBeaconPoints w y (xMin, xMax)
  | xMin > xMax = []
  | otherwise = case sensorXSkipPoint w (xMin, y) of
      Just (x', _) -> potentialBeaconPoints w y (x', xMax)
      Nothing -> (xMin, y) : potentialBeaconPoints w y (xMin + 1, xMax)

countNoBeacon :: World -> Int -> Int
countNoBeacon w@(((xMin, _), (xMax, _)), _) y = xMax - (xMin + length (potentialBeaconPoints w y (xMin, xMax)))

findBeacon :: (Int, Int) -> World -> Coordinate
findBeacon (lower, upper) w@(_, m) = head $ concat [filter (\p -> isNothing $ M.lookup p m) $ potentialBeaconPoints w y (lower, upper) | y <- [lower .. upper]]
