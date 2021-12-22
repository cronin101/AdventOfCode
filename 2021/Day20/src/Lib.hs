{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib (loadInput, showImage, step, pixelCount, enhance) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified GHC.ResponseFile as A

type EnhancementAlgorithm = IM.IntMap Bool

type Coordinate = (Int, Int)

-- (BackgroundColour, Pixels)
type Image = (Bool, M.Map Coordinate Bool)

type State = (S.Set Coordinate, EnhancementAlgorithm, Image)

charsToBools :: BSC.ByteString -> [Bool]
charsToBools = map (== '#') . BSC.unpack

-- >>> A.parseOnly parseEnhancementAlgorithm "..#.#..#####.#.#.#"
-- Right (fromList [(0,False),(1,False),(2,True),(3,False),(4,True),(5,False),(6,False),(7,True),(8,True),(9,True),(10,True),(11,True),(12,False),(13,True),(14,False),(15,True),(16,False),(17,True)])
parseEnhancementAlgorithm :: A.Parser EnhancementAlgorithm
parseEnhancementAlgorithm = do
  bits <- A.takeWhile1 (\c -> c == '#' || c == '.')
  return $ IM.fromAscList $ zip [0 ..] $ charsToBools bits

-- >>> A.parseOnly parseRow "#..#."
-- Right [True,False,False,True,False]
parseRow :: A.Parser [Bool]
parseRow = do
  bits <- A.takeWhile1 (\c -> c == '#' || c == '.')
  return $ charsToBools bits

-- >>> A.parseOnly parseRows "#..#.\n#...."
-- Right [[True,False,False,True,False],[True,False,False,False,False]]
parseRows :: A.Parser [[Bool]]
parseRows = A.sepBy1 parseRow A.endOfLine

-- >>> A.parseOnly parseInputImage "#..#.\n#...."
-- Right (fromList [((0,0),False),((0,1),True),((1,0),False),((1,1),False),((2,0),False),((2,1),False),((3,0),False),((3,1),True),((4,0),False),((4,1),False)])
parseInputImage :: A.Parser Image
parseInputImage = do
  rows <- parseRows
  let height = length rows
  let ys = reverse [0 .. height -1]
  return $ (False, M.fromList $ concatMap reorderWithCoordinate $ zip ys $ map (zip [0 ..]) rows)
  where
    reorderWithCoordinate (y, row) = map (\(x, val) -> ((x, y), val)) row

parseInput :: A.Parser State
parseInput = do
  enhancementAlgorithm <- parseEnhancementAlgorithm
  A.count 2 A.endOfLine
  image <- parseInputImage
  return (M.keysSet $ snd image, enhancementAlgorithm, image)

-- >>> loadInput "example.txt"
-- (fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),(2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)],fromList [(0,False),(1,False),(2,True),(3,False),(4,True),(5,False),(6,False),(7,True),(8,True),(9,True),(10,True),(11,True),(12,False),(13,True),(14,False),(15,True),(16,False),(17,True),(18,False),(19,True),(20,True),(21,True),(22,False),(23,True),(24,True),(25,False),(26,False),(27,False),(28,False),(29,False),(30,True),(31,True),(32,True),(33,False),(34,True),(35,True),(36,False),(37,True),(38,False),(39,False),(40,True),(41,True),(42,True),(43,False),(44,True),(45,True),(46,True),(47,True),(48,False),(49,False),(50,True),(51,True),(52,True),(53,True),(54,True),(55,False),(56,False),(57,True),(58,False),(59,False),(60,False),(61,False),(62,True),(63,False),(64,False),(65,True),(66,False),(67,False),(68,True),(69,True),(70,False),(71,False),(72,True),(73,True),(74,True),(75,False),(76,False),(77,True),(78,True),(79,True),(80,True),(81,True),(82,True),(83,False),(84,True),(85,True),(86,True),(87,False),(88,False),(89,False),(90,True),(91,True),(92,True),(93,True),(94,False),(95,False),(96,True),(97,False),(98,False),(99,True),(100,True),(101,True),(102,True),(103,True),(104,False),(105,False),(106,True),(107,True),(108,False),(109,False),(110,True),(111,False),(112,True),(113,True),(114,True),(115,True),(116,True),(117,False),(118,False),(119,False),(120,True),(121,True),(122,False),(123,True),(124,False),(125,True),(126,False),(127,False),(128,True),(129,False),(130,True),(131,True),(132,False),(133,False),(134,True),(135,False),(136,True),(137,False),(138,False),(139,False),(140,False),(141,False),(142,False),(143,True),(144,False),(145,True),(146,True),(147,True),(148,False),(149,True),(150,True),(151,True),(152,True),(153,True),(154,True),(155,False),(156,True),(157,True),(158,True),(159,False),(160,True),(161,True),(162,True),(163,True),(164,False),(165,False),(166,False),(167,True),(168,False),(169,True),(170,True),(171,False),(172,True),(173,True),(174,False),(175,False),(176,True),(177,False),(178,False),(179,True),(180,False),(181,False),(182,True),(183,True),(184,True),(185,True),(186,True),(187,False),(188,False),(189,False),(190,False),(191,False),(192,True),(193,False),(194,True),(195,False),(196,False),(197,False),(198,False),(199,True),(200,True),(201,True),(202,False),(203,False),(204,True),(205,False),(206,True),(207,True),(208,False),(209,False),(210,False),(211,False),(212,False),(213,False),(214,True),(215,False),(216,False),(217,False),(218,False),(219,False),(220,True),(221,False),(222,False),(223,True),(224,False),(225,False),(226,True),(227,False),(228,False),(229,True),(230,True),(231,False),(232,False),(233,True),(234,False),(235,False),(236,False),(237,True),(238,True),(239,False),(240,True),(241,True),(242,True),(243,True),(244,True),(245,True),(246,False),(247,True),(248,True),(249,True),(250,True),(251,False),(252,True),(253,True),(254,True),(255,True),(256,False),(257,True),(258,False),(259,True),(260,False),(261,False),(262,False),(263,True),(264,False),(265,False),(266,False),(267,False),(268,False),(269,False),(270,False),(271,True),(272,False),(273,False),(274,True),(275,False),(276,True),(277,False),(278,True),(279,False),(280,False),(281,False),(282,True),(283,True),(284,True),(285,True),(286,False),(287,True),(288,True),(289,False),(290,True),(291,False),(292,False),(293,False),(294,False),(295,False),(296,False),(297,True),(298,False),(299,False),(300,True),(301,False),(302,False),(303,False),(304,True),(305,True),(306,False),(307,True),(308,False),(309,True),(310,True),(311,False),(312,False),(313,True),(314,False),(315,False),(316,False),(317,True),(318,True),(319,False),(320,True),(321,False),(322,True),(323,True),(324,False),(325,False),(326,True),(327,True),(328,True),(329,False),(330,True),(331,False),(332,False),(333,False),(334,False),(335,False),(336,False),(337,True),(338,False),(339,True),(340,False),(341,False),(342,False),(343,False),(344,False),(345,False),(346,False),(347,True),(348,False),(349,True),(350,False),(351,True),(352,False),(353,True),(354,True),(355,True),(356,True),(357,False),(358,True),(359,True),(360,True),(361,False),(362,True),(363,True),(364,False),(365,False),(366,False),(367,True),(368,False),(369,False),(370,False),(371,False),(372,False),(373,True),(374,True),(375,True),(376,True),(377,False),(378,True),(379,False),(380,False),(381,True),(382,False),(383,False),(384,True),(385,False),(386,True),(387,True),(388,False),(389,True),(390,False),(391,False),(392,False),(393,False),(394,True),(395,True),(396,False),(397,False),(398,True),(399,False),(400,True),(401,True),(402,True),(403,True),(404,False),(405,False),(406,False),(407,False),(408,True),(409,True),(410,False),(411,False),(412,False),(413,True),(414,True),(415,False),(416,False),(417,True),(418,False),(419,False),(420,False),(421,True),(422,False),(423,False),(424,False),(425,False),(426,False),(427,False),(428,True),(429,False),(430,True),(431,False),(432,False),(433,False),(434,False),(435,False),(436,False),(437,False),(438,True),(439,False),(440,False),(441,False),(442,False),(443,False),(444,False),(445,False),(446,True),(447,True),(448,False),(449,False),(450,True),(451,True),(452,True),(453,True),(454,False),(455,False),(456,True),(457,False),(458,False),(459,False),(460,True),(461,False),(462,True),(463,False),(464,True),(465,False),(466,False),(467,False),(468,True),(469,True),(470,False),(471,False),(472,True),(473,False),(474,True),(475,False),(476,False),(477,True),(478,True),(479,True),(480,False),(481,False),(482,True),(483,True),(484,True),(485,True),(486,True),(487,False),(488,False),(489,False),(490,False),(491,False),(492,False),(493,False),(494,False),(495,True),(496,False),(497,False),(498,True),(499,True),(500,True),(501,True),(502,False),(503,False),(504,False),(505,False),(506,False),(507,False),(508,True),(509,False),(510,False),(511,True)],fromList [((0,0),False),((0,1),False),((0,2),True),((0,3),True),((0,4),True),((1,0),False),((1,1),False),((1,2),True),((1,3),False),((1,4),False),((2,0),True),((2,1),True),((2,2),False),((2,3),False),((2,4),False),((3,0),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((4,0),True),((4,1),False),((4,2),True),((4,3),False),((4,4),False)])
loadInput :: [Char] -> IO State
loadInput fileName =
  fromRight (S.empty, IM.empty, (False, M.empty))
    . A.parseOnly parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> kernel (0,0)
-- [(-1,1),(0,1),(1,1),(-1,0),(0,0),(1,0),(-1,-1),(0,-1),(1,-1)]
kernel :: Coordinate -> [Coordinate]
kernel (x, y) = [(x + dx, y + dy) | dy <- reverse [-1 .. 1], dx <- [-1 .. 1]]

nextBgColour algorithm (bgColour, _) = kernelValue algorithm (bgColour, M.fromList [((x, y), bgColour) | x <- [-1 .. 1], y <- [-1 .. 1]]) (0, 0)

kernelValue :: EnhancementAlgorithm -> Image -> Coordinate -> Bool
kernelValue algorithm (bgColour, pixels) p = algorithm IM.! index
  where
    index = sum $ zipWith (*) (iterate (* 2) 1) $ reverse $ map (if' 1 0 . fromMaybe bgColour . (`M.lookup` pixels)) $ kernel p
    if' t _ True = t
    if' _ f False = f

-- >>> adjacents (0,0)
-- [(-1,1),(0,1),(1,1),(-1,0),(1,0),(-1,-1),(0,-1),(1,-1)]
adjacents :: Coordinate -> S.Set Coordinate
adjacents p = S.fromList $ filter (/= p) $ kernel p

step :: State -> State
step (lastChanged, enhancementAlgorithm, image@(bgColour, pixels)) = (nextChanged, enhancementAlgorithm, (bgColour', pixels'))
  where
    bgColour' = nextBgColour enhancementAlgorithm image
    toUpdate = S.unions $ S.map adjacents $ S.unions $ S.map adjacents lastChanged
    update = M.unions $ map (\p -> M.singleton p (kernelValue enhancementAlgorithm image p)) $ S.toList toUpdate
    pixels' = M.union update pixels
    nextChanged = S.filter (\p -> M.lookup p pixels /= M.lookup p pixels') toUpdate

-- >>> (\(_, a, i) -> kernelValue a i (2,2)) <$> loadInput "example.txt"
-- True

bounds :: Image -> ((Int, Int), (Int, Int))
bounds (_, pixels) = ((xmin, ymin), (xmax, ymax))
  where
    points = M.keysSet pixels
    ymax = S.findMax $ S.map snd points
    ymin = S.findMin $ S.map snd points
    xmax = S.findMax $ S.map fst points
    xmin = S.findMin $ S.map fst points

-- >>> showImage <$> loadInput "example.txt"
-- "#..#.\n#....\n##..#\n..#..\n..###"
showImage :: State -> String
showImage (_, _, image@(_, pixels)) = intercalate "\n" [concat [if M.lookup (x, y) pixels == Just True then "█" else " " | x <- [xmin .. xmax]] | y <- reverse [ymin .. ymax]]
  where
    ((xmin, ymin), (xmax, ymax)) = bounds image

-- >>> pixelCount . step . step <$> loadInput "example.txt"
-- 35
pixelCount :: State -> Int
pixelCount (_, _, (_, image)) = (length . filter id . M.elems) image

-- >>> pixelCount . step . step <$> loadInput "input.txt"
-- 5413

enhance = (!! 50) . iterate step
