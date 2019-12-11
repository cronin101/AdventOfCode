import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Ord
import           Debug.Trace

getDimensionsOfMap :: [String] -> (Int, Int)
getDimensionsOfMap map = (length . head $ map, length map)

readAsteroids :: [String] -> (Int, Int) -> [(Int, Int)] -> Map (Int, Int) Bool
readAsteroids remainingMap (x, y) knownAsteroids =
  case remainingMap of
    [] -> Map.fromList $ zip knownAsteroids (repeat True)
    "":restOfMap -> readAsteroids restOfMap (0, y + 1) knownAsteroids
    (char:restOfLine):restOfMap ->
      readAsteroids
        (restOfLine : restOfMap)
        (x + 1, y)
        (case char of
           '.' -> knownAsteroids
           '#' -> (x, y) : knownAsteroids)

findVisibleAsteroids :: Map (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
findVisibleAsteroids asteroidMap source =
  filter (lineIsUnobstructed source) $
  filter (/= source) . map fst $ Map.toList asteroidMap
  where
    isAsteroid coordinate = Map.lookup coordinate asteroidMap == Just True
    lineIsUnobstructed a@(x1, y1) b@(x2, y2) =
      not $ any isAsteroid pointsBetween
      where
        pointsBetween =
          filter (/= (x2, y2)) $
          filter (/= (x1, y1)) $
          case (x2 - x1, y2 - y1) of
            (_, 0) -> [(x, y1) | x <- xRange]
            (0, _) -> [(x1, y) | y <- yRange]
            _ ->
              [ (x, y)
              | x <- xRange
              , y <- yRange
              , toRational y == (m * toRational x) + c
              ]
        m = toRational (y2 - y1) / toRational (x2 - x1)
        c = toRational y1 - (m * toRational x1)
        xRange = [xMin .. xMax]
        yRange = [yMin .. yMax]
        [xMin, xMax] = sort [x1, x2]
        [yMin, yMax] = sort [y1, y2]

main :: IO ()
main = do
  mapString <- lines <$> readFile "./input.txt"
  let asteroidMap = readAsteroids mapString (0, 0) []
  let asteroids = map fst $ Map.toList asteroidMap
  putStrLn "Part One:"
  print $
    maximumBy (comparing snd) $
    map (\a -> (a, length $ findVisibleAsteroids asteroidMap a)) asteroids
