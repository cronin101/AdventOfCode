{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib (virtualVolume, virtualSimulate, loadInput) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S

type Coord3D a = (a, a, a)

type PhysicalReactor a = S.Set (Coord3D a)

type Cuboid a = (Coord3D a, Coord3D a)

type Instruction a = (Bool, Cuboid a)

type VirtualReactor a = S.Set (Cuboid a)

-- >>> A.parseOnly parseInstruction "on x=10..12,y=10..12,z=10..12"
-- Right (True,((10,10,10),(12,12,12)))
parseInstruction :: Integral a => A.Parser (Instruction a)
parseInstruction = do
  toggle <- "on" <|> "off"
  " x="
  xmin <- A.signed A.decimal
  ".."
  xmax <- A.signed A.decimal
  ",y="
  ymin <- A.signed A.decimal
  ".."
  ymax <- A.signed A.decimal
  ",z="
  zmin <- A.signed A.decimal
  ".."
  zmax <- A.signed A.decimal
  return (toggle == "on", ((xmin, ymin, zmin), (xmax, ymax, zmax)))

overlap :: Integral a => Cuboid a -> Cuboid a -> Cuboid a
overlap ((xmin, ymin, zmin), (xmax, ymax, zmax)) ((xmin', ymin', zmin'), (xmax', ymax', zmax')) = ((max xmin xmin', max ymin ymin', max zmin zmin'), (min xmax xmax', min ymax ymax', min zmax zmax'))

union :: Integral a => Cuboid a -> Cuboid a -> [Cuboid a]
union a b = [overlap a b]

volume :: Integral a => Cuboid a -> a
volume ((xmin, ymin, zmin), (xmax, ymax, zmax)) = (abs (xmax - xmin) + 1) * (abs (ymax - ymin) + 1) * (abs (zmax - zmin) + 1)

parseInstructions :: Integral a => A.Parser [Instruction a]
parseInstructions = A.sepBy1 parseInstruction A.endOfLine

-- >>> loadInput "smallExample.txt"
-- [(True,((10,10,10),(12,12,12))),(True,((11,11,11),(13,13,13))),(False,((9,9,9),(11,11,11))),(True,((10,10,10),(10,10,10)))]
loadInput :: Integral a => [Char] -> IO [Instruction a]
loadInput fileName =
  fromRight []
    . A.parseOnly parseInstructions
    <$> BSC.readFile
      ("src/" ++ fileName)

boundedPoints :: Integral a => Cuboid a -> [Coord3D a]
boundedPoints cuboid = [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax]]
  where
    ((xmin, ymin, zmin), (xmax, ymax, zmax)) = overlap cuboid ((-50, -50, -50), (50, 50, 50))

doesIntersect :: Integral a => Cuboid a -> Cuboid a -> Bool
doesIntersect a b = (> 0) . volume $ overlap a b

physicalEvaluate :: Integral a => PhysicalReactor a -> Instruction a -> PhysicalReactor a
physicalEvaluate r (True, cuboid) = S.union (S.fromList $ boundedPoints cuboid) r
physicalEvaluate r (False, cuboid) = r S.\\ S.fromList (boundedPoints cuboid)

physicalSimulate :: Integral a => [Instruction a] -> PhysicalReactor a
physicalSimulate = foldl physicalEvaluate S.empty

-- >>> S.size . physicalSimulate <$> loadInput "smallExample.txt"
-- 39

-- >>> S.size . physicalSimulate <$> loadInput "largerExample.txt"
-- 590784

-- >>> S.size . physicalSimulate <$> loadInput "input.txt"
-- 553201

normalizedReactor :: Integral a => VirtualReactor a -> Cuboid a -> VirtualReactor a
normalizedReactor r c = S.unions $ S.map (\c' -> S.fromList $ snd $ normalize (c, c')) r

normalizedCuboid :: Integral a => VirtualReactor a -> Cuboid a -> S.Set (Cuboid a)
normalizedCuboid r c = S.unions $ S.map (\c' -> S.fromList $ fst $ normalize (c, c')) r

-- >>> ((virtualEvaluate S.empty) . head)  <$> loadInput "smallExample.txt"
-- fromList [((10,10,10),(12,12,12))]
virtualEvaluate :: Integral a => VirtualReactor a -> Instruction a -> VirtualReactor a
virtualEvaluate r (True, c)
  | S.null r = S.singleton c
  | otherwise = S.union (normalizedCuboid r c) (normalizedReactor r c)
virtualEvaluate r (False, c)
  | S.null r = S.empty
  | otherwise = S.difference (normalizedReactor r c) (normalizedCuboid r c)

-- >>> (virtualVolume. virtualSimulate . take 2 ) <$> loadInput "smallExample.txt"
-- 46
virtualSimulate :: Integral a => [Instruction a] -> VirtualReactor a
virtualSimulate = foldl virtualEvaluate S.empty

-- >>> ( S.toList . virtualSimulate . take 3) <$> loadInput "smallExample.txt"
-- [((10,10,12),(10,10,12)),((10,11,12),(10,11,12)),((10,12,10),(10,12,10)),((10,12,11),(10,12,11)),((10,12,12),(10,12,12)),((11,10,12),(11,10,12)),((11,11,12),(11,11,12)),((11,11,13),(11,11,13)),((11,12,10),(11,12,10)),((11,12,11),(11,12,11)),((11,12,12),(11,12,12)),((11,12,13),(11,12,13)),((11,13,11),(11,13,11)),((11,13,12),(11,13,12)),((11,13,13),(11,13,13)),((12,10,10),(12,10,10)),((12,10,11),(12,10,11)),((12,10,12),(12,10,12)),((12,11,10),(12,11,10)),((12,11,11),(12,11,11)),((12,11,12),(12,11,12)),((12,11,13),(12,11,13)),((12,12,10),(12,12,10)),((12,12,11),(12,12,11)),((12,12,12),(12,12,12)),((12,12,13),(12,12,13)),((12,13,11),(12,13,11)),((12,13,12),(12,13,12)),((12,13,13),(12,13,13)),((13,11,11),(13,11,11)),((13,11,12),(13,11,12)),((13,11,13),(13,11,13)),((13,12,11),(13,12,11)),((13,12,12),(13,12,12)),((13,12,13),(13,12,13)),((13,13,11),(13,13,11)),((13,13,12),(13,13,12)),((13,13,13),(13,13,13))]

-- >>> (virtualVolume. virtualSimulate) <$> loadInput "virtualExample.txt"
-- 39

virtualVolume :: Integral a => VirtualReactor a -> a
virtualVolume r = sum $ map volume $ S.toList r

-- >>> normalize (((10,10,12),(10,11,12)), ((10,10,10),(10,10,10)))
-- ([((10,11,12),(10,11,12)),((10,10,12),(10,10,12))],[((10,10,10),(10,10,10))])

simplify :: Integral a => [Cuboid a] -> [Cuboid a]
simplify [] = []
simplify c =
  if volume bounding == sum (map volume c)
    then [bounding]
    else c
  where
    bounding = ((xmin, ymin, zmin), (xmax, ymax, zmax))
    xmin = minimum $ map ((\(x, y, z) -> x) . fst) c
    ymin = minimum $ map ((\(x, y, z) -> y) . fst) c
    zmin = minimum $ map ((\(x, y, z) -> z) . fst) c
    xmax = maximum $ map ((\(x, y, z) -> x) . fst) c
    ymax = maximum $ map ((\(x, y, z) -> y) . fst) c
    zmax = maximum $ map ((\(x, y, z) -> z) . fst) c

-- >>> normalize (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13)))
-- ([((11,11,11),(12,12,12)),((10,11,10),(10,12,10)),((10,10,10),(10,10,10)),((10,11,11),(10,12,12)),((10,10,11),(10,10,12)),((11,10,11),(12,10,12)),((11,10,10),(12,10,10)),((11,11,10),(12,12,10))],[((11,11,11),(12,12,12)),((13,11,11),(13,13,13)),((11,13,11),(12,13,12)),((11,13,13),(12,13,13)),((11,11,13),(12,12,13))])
normalize :: Integral a => (Cuboid a, Cuboid a) -> ([Cuboid a], [Cuboid a])
normalize (a, b) = case valid [overlap a b] of
  [overlap] -> (valid $ o : a_rest, valid $ o : b_rest)
  _ -> ([a], [b])
  where
    valid cuboids = filter (\((x, y, z), (x', y', z')) -> x <= x' && y <= y' && z <= z') cuboids
    o = overlap a b
    a_rest = rest a o
    b_rest = rest b o
    rest c@((xmin, ymin, zmin), (xmax, ymax, zmax)) o@((xmin', ymin', zmin'), (xmax', ymax', zmax')) =
      leftSide ++ rightSide ++ top ++ bottom ++ front ++ back
      where
        leftMin = xmin
        leftMax = xmin' - 1
        bottomMin = ymin
        bottomMax = ymin' - 1
        topMin = ymax' + 1
        topMax = ymax
        frontMin = zmin
        frontMax = zmin' - 1
        backMax = zmax
        backMin = zmax' + 1
        leftSideCenter =
          simplify $
            valid
              [ -- left center top
                overlap c ((leftMin, topMin, zmin'), (leftMax, topMax, zmax')),
                -- left center mid
                overlap c ((leftMin, ymin', zmin'), (leftMax, ymax', zmax')),
                -- left center bottom
                overlap c ((leftMin, bottomMin, zmin'), (leftMax, bottomMax, zmax'))
              ]
        leftSideFront =
          simplify $
            valid
              [ -- left front top
                overlap c ((leftMin, topMin, frontMin), (leftMax, topMax, frontMax)),
                -- left front mid
                overlap c ((leftMin, ymin', frontMin), (leftMax, ymax', frontMax)),
                -- left front bottom
                overlap c ((leftMin, bottomMin, frontMin), (leftMax, bottomMax, frontMax))
              ]
        leftSideBack =
          simplify $
            valid
              [ -- left back top
                overlap c ((leftMin, topMin, backMin), (leftMax, topMax, backMax)),
                -- left back mid
                overlap c ((leftMin, ymin', backMin), (leftMax, ymax', backMax)),
                -- left back bottom
                overlap c ((leftMin, backMin, frontMin), (leftMax, bottomMax, backMax))
              ]
        leftSide = simplify $ leftSideFront ++ leftSideCenter ++ leftSideBack
        rightMin = xmax' + 1
        rightMax = xmax
        rightSideCenter =
          simplify $
            valid
              [ -- right center top
                overlap c ((rightMin, topMin, zmin'), (rightMax, topMax, zmax')),
                -- right center mid
                overlap c ((rightMin, ymin', zmin'), (rightMax, ymax', zmax')),
                -- right center bottom
                overlap c ((rightMin, bottomMin, zmin'), (rightMax, bottomMax, zmax'))
              ]
        rightSideFront =
          simplify $
            valid
              [ -- right front top
                overlap c ((rightMin, topMin, frontMin), (rightMax, topMax, frontMax)),
                -- right front mid
                overlap c ((rightMin, ymin', frontMin), (rightMax, ymax', frontMax)),
                -- right front bottom
                overlap c ((rightMin, bottomMin, frontMin), (rightMax, bottomMax, frontMax))
              ]
        rightSideBack =
          simplify $
            valid
              [ -- right back top
                overlap c ((rightMin, topMin, backMin), (rightMax, topMax, backMax)),
                -- right back mid
                overlap c ((rightMin, ymin', backMin), (rightMax, ymax', backMax)),
                -- right back bottom
                overlap c ((rightMin, backMin, frontMin), (rightMax, bottomMax, backMax))
              ]
        rightSide = simplify $ rightSideFront ++ rightSideCenter ++ rightSideBack
        middleMin = xmin'
        middleMax = xmax'
        top =
          simplify $
            valid
              [ -- middle center top
                overlap c ((middleMin, topMin, zmin'), (middleMax, topMax, zmax')),
                -- middle front top
                overlap c ((middleMin, topMin, frontMin), (middleMax, topMax, frontMax)),
                -- middle back top
                overlap c ((middleMin, topMin, backMin), (middleMax, topMax, backMax))
              ]
        bottom =
          simplify $
            valid
              [ ---middle center bottom
                overlap c ((middleMin, bottomMin, zmin'), (middleMax, bottomMax, zmax')),
                -- middle front bottom
                overlap c ((middleMin, bottomMin, frontMin), (middleMax, bottomMax, frontMax)),
                -- middle back bottom
                overlap c ((middleMin, bottomMin, backMin), (middleMax, bottomMax, backMax))
              ]
        front =
          [ --- middle front mid
            overlap c ((middleMin, ymin', frontMin), (middleMax, ymax', frontMax))
          ]
        back =
          [ -- middle back min
            overlap c ((middleMin, ymin', backMin), (middleMax, ymax', backMax))
          ]

-- >>> bimap volume volume $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13)))
-- (27,27)

-- >>> bimap (map volume) (map volume) $ normalize $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13)))
-- ([8,2,1,4,2,4,2,4],[8,9,4,2,4])

-- >>> bimap (sum . map volume) (sum . map volume) $ normalize $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13)))
-- (27,27)

-- >>> S.difference (S.fromList $ boundedPoints ((10,10,10),(12,12,12))) (S.fromList $ concatMap boundedPoints $ fst $ normalize $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13))))
-- fromList []

-- >>> S.difference  (S.fromList $ concatMap boundedPoints $ fst $ normalize $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13)))) (S.fromList $ boundedPoints ((10,10,10),(12,12,12)))
-- fromList []

-- >>> S.difference (S.fromList $ boundedPoints ((11,11,11),(13,13,13))) (S.fromList $ concatMap boundedPoints $ snd $ normalize $ (((10,10,10),(12,12,12)), ((11,11,11),(13,13,13))))
-- fromList []
