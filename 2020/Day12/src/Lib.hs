{-# LANGUAGE OverloadedStrings, Strict #-}

module Lib
  ( loadInput
  , scanStates
  , initialState
  , manhattanDistance
  , NavigationStyle(Direct, Waypoint)
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Attoparsec.ByteString.Char8
                                                ( char
                                                , parseOnly
                                                , endOfLine
                                                , sepBy1
                                                , choice
                                                , inClass
                                                , isDigit
                                                , takeWhile1
                                                , Parser
                                                , satisfy
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Either                    ( fromRight )
import           Data.Angle                     ( sine
                                                , cosine
                                                , Degrees
                                                , Degrees(Degrees)
                                                )
import           Data.List                      ( scanl' )

data Instruction = North Int | South Int | West Int | East Int | LeftTurn (Degrees Int) | RightTurn (Degrees Int) | Forward Int
    deriving (Show)

type ShipPosition = (Int, Int)
type WaypointPosition = (Int, Int)
type Direction = Degrees Int

data NavigationStyle = Direct | Waypoint

data ShipState = DirectShipState ShipPosition Direction | WaypointShipState ShipPosition WaypointPosition
  deriving Show

magOpTheta
  :: (RealFrac a1, Integral a2, Integral a3, Num x)
  => a2
  -> (Degrees x -> a1)
  -> Degrees a3
  -> Int
magOpTheta magnitude op (Degrees angle) =
  round . (fromIntegral magnitude *) . op . Degrees $ fromIntegral angle

sineTheta :: (Integral a2, Integral a3) => a2 -> Degrees a3 -> Int
sineTheta m = magOpTheta m sine

cosineTheta :: (Integral a2, Integral a3) => a2 -> Degrees a3 -> Int
cosineTheta m = magOpTheta m cosine

parseDigitsAsInt :: Parser Int
parseDigitsAsInt = fst . fromJust . BSC.readInt <$> takeWhile1 isDigit

parseCardinal :: Parser Instruction
parseCardinal = do
  direction <- satisfy $ inClass "NSEW"
  distance  <- parseDigitsAsInt
  return
    (case direction of
      'N' -> North distance
      'S' -> South distance
      'E' -> East distance
      'W' -> West distance
    )

parseTurn :: Parser Instruction
parseTurn = do
  direction <- satisfy $ inClass "LR"
  degrees   <- parseDigitsAsInt
  return
    (case direction of
      'L' -> LeftTurn $ Degrees degrees
      'R' -> RightTurn $ Degrees degrees
    )

parseForward :: Parser Instruction
parseForward = do
  char 'F'
  Forward <$> parseDigitsAsInt

parseInstruction :: Parser Instruction
parseInstruction = choice [parseCardinal, parseTurn, parseForward]

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy1 parseInstruction endOfLine

nextState :: ShipState -> Instruction -> ShipState
nextState (DirectShipState r@(rx, ry) direction) instruction =
  case instruction of
    North     y     -> DirectShipState (rx, ry + y) direction
    South     y'    -> DirectShipState (rx, ry - y') direction
    East      x     -> DirectShipState (rx + x, ry) direction
    West      x'    -> DirectShipState (rx - x', ry) direction
    LeftTurn  angle -> DirectShipState r (direction - angle)
    RightTurn angle -> DirectShipState r (direction + angle)
    Forward   v     -> DirectShipState (rx + x, ry + y) direction
     where
      x = v `sineTheta` direction
      y = v `cosineTheta` direction

nextState (WaypointShipState s@(sx, sy) w@(wx, wy)) instruction =
  case instruction of
    North     y     -> WaypointShipState s (wx, wy + y)
    South     y'    -> WaypointShipState s (wx, wy - y')
    East      x     -> WaypointShipState s (wx + x, wy)
    West      x'    -> WaypointShipState s (wx - x', wy)
    Forward   v     -> WaypointShipState (sx + (v * wx), sy + (v * wy)) w
    LeftTurn  angle -> WaypointShipState s (rotate w angle)
    RightTurn angle -> WaypointShipState s (rotate w $ Degrees 0 - angle)
 where
  rotate (wx, wy) angle = (wx', wy')
   where
    wx' = wx `cosineTheta` angle - wy `sineTheta` angle
    wy' = wx `sineTheta` angle + wy `cosineTheta` angle

initialState :: NavigationStyle -> ShipState
initialState Direct   = DirectShipState (0, 0) (Degrees 90)
initialState Waypoint = WaypointShipState (0, 0) (10, 1)

scanStates :: ShipState -> [Instruction] -> [ShipState]
scanStates = scanl' nextState

manhattanDistance :: ShipState -> Int
manhattanDistance (DirectShipState   (rx, ry) _) = abs rx + abs ry
manhattanDistance (WaypointShipState (rx, ry) _) = abs rx + abs ry

loadInput :: String -> IO [Instruction]
loadInput fileName =
  fromRight [] . parseOnly parseInstructions <$> BSC.readFile
    ("src/" ++ fileName)
