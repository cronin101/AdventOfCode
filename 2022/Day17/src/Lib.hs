{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadSprites,
    startState,
    loadInput,
    step,
    heightAtScore,
    hintSolutionB,
    moveUpAndSetScore,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Ix (inRange)
import Data.List (intercalate)
import Data.Set qualified as S

type Sprite = S.Set Coordinate

-- (y, x) to make S.max magic work
type Coordinate = (Int, Int)

type Background = S.Set Coordinate

data Direction = L | R deriving (Show)

data State = State
  { background :: Background,
    sprite :: Sprite,
    nextSprites :: [Sprite],
    xRange :: (Int, Int),
    nextDirections :: [Direction],
    score :: Int
  }

instance Show State where
  show state@State {background, sprite} = intercalate "\n" [concat [if (y, x) `S.member` background then "#" else if (y, x) `S.member` sprite then "@" else "." | x <- [xMin .. xMax]] | y <- reverse [yMin .. yMax]]
    where
      ((xMin, yMin), (xMax, yMax)) = bounds state

-- >>> bounds $ startState [S.fromList [(0,0),(1,0),(2,0),(3,0)]]
-- ((0,0),(7,3))
bounds :: State -> ((Int, Int), (Int, Int))
bounds State {xRange, sprite, background} = ((xMin, 0), (xMax, yMax))
  where
    (xMin, xMax) = xRange
    yMax = maximum $ map fst $ S.toList $ S.union sprite background

-- >>> A.parseOnly parseDirection "<"
-- Right L
parseDirection :: A.Parser Direction
parseDirection = A.choice [L <$ A.char '<', R <$ A.char '>']

-- >>> A.parseOnly parseDirections ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
-- Right [R,R,R,L,L,R,L,R,R,L,L,L,R,R,L,R,R,R,L,L,L,R,R,R,L,L,L,R,L,L,L,R,R,L,R,R,L,L,R,R]
parseDirections :: A.Parser [Direction]
parseDirections = A.many1 parseDirection

-- >>> A.parseOnly parseSprite "####"
-- Right (fromList [(0,0),(1,0),(2,0),(3,0)])

-- >>> A.parseOnly parseSprite ".#.\n###\n.#."
-- Right (fromList [(0,1),(1,0),(1,1),(1,2),(2,1)])
parseSprite :: A.Parser Sprite
parseSprite = toPixelCoordinates <$> A.sepBy (A.many1 $ A.choice [False <$ A.char '.', True <$ A.char '#']) A.endOfLine

parseSprites :: A.Parser [Sprite]
parseSprites = A.sepBy parseSprite $ A.count 2 A.endOfLine

loadSprites :: IO [Sprite]
loadSprites = fromRight [] . A.parseOnly parseSprites <$> BSC.readFile "src/sprites.txt"

loadInput :: String -> IO [Direction]
loadInput = (fromRight [] . A.parseOnly parseDirections <$>) . BSC.readFile . ("src/" ++)

toPixelCoordinates :: [[Bool]] -> S.Set Coordinate
toPixelCoordinates sprite =
  S.fromList
    [ (y, x)
      | (y, row) <- zip [0 ..] $ reverse sprite,
        (x, True) <- zip [0 ..] row
    ]

startState :: [Sprite] -> [Direction] -> State
startState spriteCycle directions =
  State
    { background = S.empty,
      sprite = S.map (\(y, x) -> (y + 3, x + 2)) (head spriteCycle'),
      nextSprites = tail spriteCycle',
      xRange = (0, 6),
      nextDirections = cycle directions,
      score = 0
    }
  where
    spriteCycle' = cycle spriteCycle

-- >>> S.findMax $ S.fromList [(0,0),(1,0),(2,1),(3,0)]
-- (3,0)
step :: State -> State
step state@State {sprite, background, xRange, nextDirections, nextSprites, score}
  | S.null sprite =
      state
        { sprite = S.map (\(y, x) -> (y + 4 + fst (S.findMax background), x + 2)) $ head nextSprites,
          nextSprites = tail nextSprites
        }
  | otherwise =
      state
        { sprite = sprite',
          background = background',
          nextDirections = nextDirections',
          score = score'
        }
  where
    score' = if spriteStillInPlay then score else score + 1
    nextDirections' = tail nextDirections
    spriteStillInPlay = S.disjoint dropAttempt background && 0 <= fst (S.findMin dropAttempt)
    background' = if spriteStillInPlay then background else S.union background maybePushed
    sprite' = if spriteStillInPlay then dropAttempt else S.empty
    dropAttempt = S.map (\(y, x) -> (y - 1, x)) maybePushed
    maybePushed = if S.disjoint pushAttempt background && S.null (S.filter (not . inRange xRange . snd) pushAttempt) then pushAttempt else sprite
    pushAttempt = S.map move sprite
    move = case head nextDirections of
      L -> \(y, x) -> (y, x - 1)
      R -> \(y, x) -> (y, x + 1)

heightAtScore :: Int -> State -> Int
heightAtScore target = (\((_, _), (_, yMax)) -> yMax + 1) . bounds . head . dropWhile ((< target) . score) . iterate step

hintSolutionB :: Int -> Int -> State -> (Int, Int, State)
hintSolutionB m n s = ((\((_, _), (_, yMax)) -> yMax + 1) $ bounds state, score state, state)
  where
    state = filter (not . S.null . sprite) (iterate step s) !! max 0 (n * m)

moveUpAndSetScore :: (Int, Int) -> State -> State
moveUpAndSetScore (targetHeight, targetScore) state@State {background} = state {score = targetScore, background = S.map (\(y, x) -> (y + translationNeeded, x)) background}
  where
    translationNeeded = targetHeight - currentHeight
    ((_, _), (_, currentHeight)) = bounds state
