import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe

data Dimension
  = X
  | Y
  | Z

data Position =
  Position
    { rx :: Int
    , ry :: Int
    , rz :: Int
    }
  deriving (Show, Read, Eq, Ord)

data Velocity =
  Velocity
    { vx :: Int
    , vy :: Int
    , vz :: Int
    }
  deriving (Show, Read, Eq, Ord)

data Moon =
  Moon
    { position :: Position
    , velocity :: Velocity
    }
  deriving (Show, Read, Eq, Ord)

toEnergy :: Moon -> Int
toEnergy moon = potential * kinetic
  where
    (Position rx ry rz) = position moon
    (Velocity vx vy vz) = velocity moon
    potential = abs rx + abs ry + abs rz
    kinetic = abs vx + abs vy + abs vz

addV :: Velocity -> Velocity -> Velocity
addV (Velocity x1 y1 z1) (Velocity x2 y2 z2) =
  Velocity (x1 + x2) (y1 + y2) (z1 + z2)

createMoonPairs :: [Moon] -> [(Moon, Moon)]
createMoonPairs moons = [(m1, m2) | m1 <- moons, m2 <- moons, m1 /= m2]

updateVelocities :: [Moon] -> [Moon]
updateVelocities moons = map (updateVelocity moonPairs) moons
  where
    moonPairs = createMoonPairs moons

updatePositions :: [Moon] -> [Moon]
updatePositions = map updatePosition
  where
    updatePosition moon@(Moon (Position rx ry rz) (Velocity vx vy vz)) =
      moon {position = Position (rx + vx) (ry + vy) (rz + vz)}

-- move a towards b by a step of 1
moveTowards :: Int -> Int -> Int
moveTowards b a
  | a > b = -1
  | a < b = 1
  | otherwise = 0

-- Given a moon and a list of all moon pairs, adds the velocity diff
updateVelocity :: [(Moon, Moon)] -> Moon -> Moon
updateVelocity moonPairs moon =
  moon {velocity = addV (velocity moon) velocityDiff}
  where
    velocityDiff =
      foldr (addV . toVelocityDiff) (Velocity 0 0 0) $
      filter ((moon ==) . fst) moonPairs
    toVelocityDiff (subject@(Moon (Position sx sy sz) _), object@(Moon (Position ox oy oz) _)) =
      Velocity (moveTowards ox sx) (moveTowards oy sy) (moveTowards oz sz)

newMoon :: Position -> Moon
newMoon start = Moon start (Velocity 0 0 0)

moonParser :: String -> Moon
moonParser input = newMoon $ Position x y z
  where
    [x, y, z] = map (read . filter (`elem` ['-' .. '9'])) $ splitOn "," input

positionAndVelocityForDimension :: Dimension -> Moon -> (Int, Int)
positionAndVelocityForDimension d moon =
  case d of
    X -> (rx . position $ moon, vx . velocity $ moon)
    Y -> (ry . position $ moon, vy . velocity $ moon)
    Z -> (rz . position $ moon, vz . velocity $ moon)

-- Periodic orbits repeat the initial state and dimensions are independent, thanks Reddit.
findCycleLengthForDimension :: [[Moon]] -> Dimension -> Int
findCycleLengthForDimension (f:fs) d =
  1 +
  length
    (takeWhile
       ((/= map (positionAndVelocityForDimension d) f) .
        map (positionAndVelocityForDimension d))
       fs)

main :: IO ()
main = do
  moons <- map moonParser . lines <$> readFile "./Day12/input.txt"
  let future = iterate (updatePositions . updateVelocities) moons
  let energy = sum $ map toEnergy $ future !! 1000
  putStrLn "Part 1:"
  print energy
  putStrLn "Part 2:"
  print $ foldl1 lcm $ map (findCycleLengthForDimension future) [X, Y, Z]
