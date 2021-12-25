{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib (virtualVolume, virtualSimulate, loadInput, physicalVolume, physicalSimulate) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S

type Coord3D a = (a, a, a)

setX x (_, y, z) = (x, y, z)

setY y (x, _, z) = (x, y, z)

setZ z (x, y, _) = (x, y, z)

type PhysicalReactor a = S.Set (Coord3D a)

type Cuboid a = (Coord3D a, Coord3D a)

type Instruction a = (Bool, Cuboid a)

type VirtualReactor a = S.Set (Cuboid a)

-- >>> A.parseOnly parseInstruction "on x=10..12,y=10..12,z=10..12"
-- Right (True,((10,10,10),(12,12,12)))
parseInstruction :: Integral a => A.Parser (Instruction a)
parseInstruction = do
  toggle <- ("on" <|> "off") <* " "
  xmin <- "x=" *> A.signed A.decimal
  xmax <- ".." *> A.signed A.decimal
  ymin <- ",y=" *> A.signed A.decimal
  ymax <- ".." *> A.signed A.decimal
  zmin <- ",z=" *> A.signed A.decimal
  zmax <- ".." *> A.signed A.decimal
  return (toggle == "on", ((xmin, ymin, zmin), (xmax, ymax, zmax)))

intersection :: Integral a => Cuboid a -> Cuboid a -> Maybe (Cuboid a)
intersection ((xmin, ymin, zmin), (xmax, ymax, zmax)) ((xmin', ymin', zmin'), (xmax', ymax', zmax')) = if ox <= ox' && oy <= oy' && oz <= oz' then Just overlap else Nothing
  where
    overlap@((ox, oy, oz), (ox', oy', oz')) = ((max xmin xmin', max ymin ymin', max zmin zmin'), (min xmax xmax', min ymax ymax', min zmax zmax'))

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

boundedPoints :: Integral a => Cuboid a -> S.Set (Coord3D a)
boundedPoints cuboid =
  case intersection cuboid bounds of
    Just ((xmin, ymin, zmin), (xmax, ymax, zmax)) -> S.fromList [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax]]
    _ -> S.empty
  where
    bounds = ((-50, -50, -50), (50, 50, 50))

doesIntersect :: Integral a => Cuboid a -> Cuboid a -> Bool
doesIntersect a b = isJust $ intersection a b

physicalEvaluate :: Integral a => PhysicalReactor a -> Instruction a -> PhysicalReactor a
physicalEvaluate r (True, cuboid) = S.union (boundedPoints cuboid) r
physicalEvaluate r (False, cuboid) = r S.\\ boundedPoints cuboid

physicalSimulate :: Integral a => [Instruction a] -> PhysicalReactor a
physicalSimulate = foldl physicalEvaluate S.empty

-- >>> ((virtualEvaluate S.empty) . head)  <$> loadInput "smallExample.txt"
-- fromList [((10,10,10),(12,12,12))]
virtualEvaluate :: Integral a => VirtualReactor a -> Instruction a -> VirtualReactor a
virtualEvaluate r (True, inserted)
  | S.null r = S.singleton inserted
  | otherwise = S.union (S.singleton inserted) $ S.unions $ S.map leftover r
  where
    leftover existing = case intersection inserted existing of
      Just duplicate -> existing `subtractCuboid` duplicate
      Nothing -> S.singleton existing
virtualEvaluate r (False, c)
  | S.null r = S.empty
  | otherwise = S.unions $ S.map (`subtractCuboid` c) r

virtualSimulate :: Integral a => [Instruction a] -> VirtualReactor a
virtualSimulate = foldl virtualEvaluate S.empty

physicalVolume :: Integral a => PhysicalReactor a -> Int
physicalVolume = S.size

virtualVolume :: Integral a => VirtualReactor a -> a
virtualVolume r = sum $ map volume $ S.toList r

-- >>> subtractCuboid ((10,10,10),(12,12,12)) ((11,11,11),(13,13,13))
-- fromList [((10,10,10),(10,10,10)),((10,10,11),(10,10,12)),((10,11,10),(10,12,10)),((10,11,11),(10,12,12)),((11,10,10),(12,10,10)),((11,10,11),(12,10,12)),((11,11,10),(12,12,10))]
subtractCuboid :: Integral a => Cuboid a -> Cuboid a -> S.Set (Cuboid a)
subtractCuboid a b = case intersection a b of
  Nothing -> S.singleton a
  Just overlap
    | overlap == a -> S.empty
    | otherwise -> S.delete overlap (S.fromList $ parts a overlap)
    where
      parts (min, max) ((xmin', ymin', zmin'), (xmax', ymax', zmax')) =
        catMaybes ([front, middleForFrontBack, back] <*> catMaybes ([top, middleForTopBottom, bottom] <*> catMaybes ([left, middleForLeftRight, right] <*> [(min, max)])))
        where
          [left, middleForLeftRight, right] =
            map
              (uncurry subsetFactory)
              [ (id, setX (xmin' - 1)),
                (setX xmin', setX xmax'),
                (setX $ xmax' + 1, id)
              ]
          [top, middleForTopBottom, bottom] =
            map
              (uncurry subsetFactory)
              [ (setY $ ymax' + 1, id),
                (setY ymin', setY ymax'),
                (id, setY (ymin' - 1))
              ]
          [front, middleForFrontBack, back] =
            map
              (uncurry subsetFactory)
              [ (setZ $ zmax' + 1, id),
                (setZ zmin', setZ zmax'),
                (id, setZ $ zmin' - 1)
              ]
          subsetFactory fMin fMax = (`intersection` (fMin min, fMax max))
