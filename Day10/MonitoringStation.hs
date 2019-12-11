import           Data.Complex
import qualified Data.Complex.Polar as Polar
import           Data.List
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Data.Ord
import           Debug.Trace

getDimensionsOfMap :: [String] -> (Int, Int)
getDimensionsOfMap map = (length . head $ map, length map)

getAngle :: RealFloat a => (Int, Int) -> (Int, Int) -> a
getAngle (x, y) (x', y') =
  Polar.phase $
  Polar.fromComplex $ fromIntegral (y' - y) :+ fromIntegral (x' - x)

getDistance :: Floating a => (Int, Int) -> (Int, Int) -> a
getDistance (x, y) (x', y') =
  sqrt . fromIntegral $ ((y' - y) ^ 2 + (x' - x) ^ 2)

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
           '#' -> (x, y) : knownAsteroids
           e   -> error $ e : "")

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween a@(x1, y1) b@(x2, y2) =
  filter (a /=) $
  case (x2 - x1, y2 - y1) of
    (_, 0) -> [(x, y1) | x <- xRange]
    (0, _) -> [(x1, y) | y <- yRange]
    _ ->
      [ (x, y)
      | x <- xRange
      , y <- yRange
      , toRational y == (m * toRational x) + c
      ]
  where
    m = toRational (y2 - y1) / toRational (x2 - x1)
    c = toRational y1 - (m * toRational x1)
    xRange = [x1,x1 + signum (x2 - x1) .. x2]
    yRange = [y1,y1 + signum (y2 - y1) .. y2]

findFirstObstruction ::
     Map (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findFirstObstruction asteroidMap origin target =
  find isAsteroid $ pointsBetween origin target
  where
    isAsteroid coordinate = Map.lookup coordinate asteroidMap == Just True

findVisibleAsteroids :: Map (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
findVisibleAsteroids asteroidMap source =
  filter (lineIsUnobstructed source) $
  filter (/= source) . map fst $ Map.toList asteroidMap
  where
    lineIsUnobstructed a b = Just b == findFirstObstruction asteroidMap a b

getFirstNDestructions :: Int -> (Int, Int) -> [String] -> [(Int, Int)]
getFirstNDestructions n origin@(x, _) mapString =
  getDestructions
    (getLaserTargetCycle origin (map fst $ Map.toList initialMap))
    initialMap
    []
  where
    getDestructions (cycleHead:restCycle) remainingMap destructions
      | length destructions == n = destructions
      | otherwise =
        case findFirstObstruction remainingMap origin cycleHead of
          Just destroyed ->
            getDestructions
              restCycle
              (Map.delete destroyed remainingMap)
              (destroyed : destructions)
          Nothing -> getDestructions restCycle remainingMap destructions
    dimensions@(xLength, yLength) = getDimensionsOfMap mapString
    initialMap = readAsteroids mapString (0, 0) []

getLaserTargetCycle :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getLaserTargetCycle origin asteroids =
  cycle $
  map fst $
  nubBy (\x y -> (fst . snd) x == (fst . snd) y) $
  sortOn (Down . snd) asteroidsWithPolarCoordinates
  where
    asteroidsWithPolarCoordinates =
      map (\a -> (a, (getAngle origin a, getDistance origin a))) $
      filter (/= origin) asteroids

main :: IO ()
main = do
  mapString <- lines <$> readFile "./input.txt"
  let asteroidMap = readAsteroids mapString (0, 0) []
  let asteroids = map fst $ Map.toList asteroidMap
  putStrLn "Part One:"
  let p1@(monitoringStation, _) =
        maximumBy (comparing snd) $
        map (\a -> (a, length $ findVisibleAsteroids asteroidMap a)) asteroids
  print p1
  putStrLn "Part Two:"
  print $ head $ getFirstNDestructions 200 monitoringStation mapString
