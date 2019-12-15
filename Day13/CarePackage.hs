import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           IntCode.Intcode

data GameState =
  GameState
    { ball       :: Maybe (Int, Int)
    , paddle     :: Maybe (Int, Int)
    , score      :: Maybe Int
    , blocksLeft :: Maybe Int
    }
  deriving (Show)

toPixelMap :: [Int] -> Map (Int, Int) Int
toPixelMap outputs = buildPixelMap Map.empty (reverse outputs)
  where
    buildPixelMap m [] = m
    buildPixelMap m (x:y:val:rest) =
      buildPixelMap (Map.insert (x, y) val m) rest

getGameState :: State -> GameState
getGameState state = GameState ball paddle score blocksLeft
  where
    pixelMap = toPixelMap $ outputs state
    blocksLeft =
      case score of
        Nothing -> Nothing
        _ -> Just $ length $ filter ((== 2) . snd) $ Map.toList pixelMap
    score = Map.lookup (-1, 0) pixelMap
    ball = fmap fst $ find ((== 4) . snd) $ Map.toList pixelMap
    paddle = fmap fst $ find ((== 3) . snd) $ Map.toList pixelMap

playGame :: State -> State
playGame state = processProgram $ state {inputs = [diff]}
  where
    GameState ball paddle _ _ = getGameState state
    diff =
      case (ball, paddle) of
        (Nothing, _)               -> 0
        (_, Nothing)               -> 0
        (Just (b, _), Just (p, _)) -> signum (b - p)

main :: IO ()
main = do
  tape <-
    map (read :: String -> Int) . splitOn "," <$> readFile "./Day13/input.txt"
  putStrLn "Part One:"
  let pixelMap = toPixelMap $ outputs $ processProgram $ newProgram tape []
  print $ length $ filter ((== 2) . snd) $ Map.toList pixelMap
  putStrLn "Part Two:"
  let newGame = newProgram (2 : tail tape) []
  print $
    head $
    dropWhile ((> 0) . fromMaybe 1 . blocksLeft) $
    map getGameState $ iterate playGame newGame
