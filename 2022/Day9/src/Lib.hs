{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    solutionA,
    solutionB,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Set qualified as S

-- A rope has a head and a tail
type Rope = ((Int, Int), (Int, Int))

-- A rope with many knots
type LongRope = ((Int, Int), [(Int, Int)])

data Step = U | D | L | R deriving (Show, Read)

type StepCount = (Step, Int)

isTouching :: Rope -> Bool
isTouching r = round (distance r) <= (1 :: Integer)

distance :: Rope -> Float
distance ((x, y), (x', y')) = sqrt (x'' + y'')
  where
    x'' = (fromIntegral x - fromIntegral x') ** 2
    y'' = (fromIntegral y - fromIntegral y') ** 2

updateTail :: Rope -> Rope
updateTail rope@(h, t)
  | isTouching rope = rope
  | otherwise = (h, moveTail h t)
  where
    moveTail (x, y) (x', y')
      | x == x' = (x, round (fromIntegral (y + y') / 2 :: Float))
      | y == y' = (round (fromIntegral (x + x') / 2 :: Float), y)
      | otherwise = (x' + signum (x - x'), y' + signum (y - y'))

updateHead :: Rope -> Step -> Rope
updateHead ((x, y), t) step = (h', t)
  where
    h' = case step of
      U -> (x, y + 1)
      D -> (x, y - 1)
      L -> (x - 1, y)
      R -> (x + 1, y)

updateRope :: Rope -> Step -> Rope
updateRope r = updateTail . updateHead r

updateLongRope :: LongRope -> Step -> LongRope
updateLongRope r step = updateLongTail $ updateLongHead r
  where
    updateLongHead (h, t) = (fst $ updateHead (h, head t) step, t)
    updateLongTail (h, t) = (h, map snd $ zipWith (curry updateTail) (h : t) t)

expandStepCount :: StepCount -> [Step]
expandStepCount (step, count) = replicate count step

newRope :: Rope
newRope = ((0, 0), (0, 0))

newLongRope :: LongRope
newLongRope = ((0, 0), replicate 9 (0, 0))

-- >>> A.parseOnly parseStep "U"
-- Right U
parseStep :: A.Parser Step
parseStep = read . BSC.unpack <$> A.choice ["U", "D", "L", "R"]

-- >>> A.parseOnly parseStepCount "U 10"
-- Right (U,10)
parseStepCount :: A.Parser StepCount
parseStepCount = do
  step <- parseStep <* A.skipSpace
  count <- A.decimal
  return (step, count)

parseInput :: A.Parser [StepCount]
parseInput = A.sepBy1 parseStepCount A.endOfLine

loadInput :: String -> IO [StepCount]
loadInput = (fromRight [] . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

moveRopeWithHistory :: Rope -> [StepCount] -> [Rope]
moveRopeWithHistory r sc = scanl updateRope r (concatMap expandStepCount sc)

moveLongRopeWithHistory :: LongRope -> [StepCount] -> [LongRope]
moveLongRopeWithHistory r sc = scanl updateLongRope r (concatMap expandStepCount sc)

tailPositions :: [Rope] -> S.Set (Int, Int)
tailPositions = S.fromList . map snd

longTailPositions :: [LongRope] -> S.Set (Int, Int)
longTailPositions = S.fromList . map (last . snd)

solutionA :: [StepCount] -> Int
solutionA = S.size . tailPositions . moveRopeWithHistory newRope

solutionB :: [StepCount] -> Int
solutionB = S.size . longTailPositions . moveLongRopeWithHistory newLongRope
