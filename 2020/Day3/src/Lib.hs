{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , treeCountForSlope
  ) where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( ByteString )
import           Data.List                      ( iterate' )

data WorldMap = WorldMap
  { startPosition :: (Int, Int)
  , treePositions :: Set (Int, Int)
  , width         :: Int
  , height        :: Int
  }
  deriving Show

treesInRow :: Int -> ByteString -> Set (Int, Int)
treesInRow y bytestring =
  Set.fromDistinctAscList [ (x, y) | x <- BSC.elemIndices '#' bytestring ]

loadRows :: [ByteString] -> WorldMap
loadRows rows@(firstRow : _) = WorldMap startPosition
                                        treePositions
                                        width
                                        height
 where
  startPosition = (0, 0)
  treePositions =
    Set.unions [ treesInRow y bytestring | (y, bytestring) <- zip [0 ..] rows ]
  width  = BSC.length firstRow
  height = length rows

generatePath :: WorldMap -> (Int, Int) -> [(Int, Int)]
generatePath map slope =
  takeWhile ((< height map) . snd)
    $ iterate' (`addTuples` slope)
    $ startPosition map
  where addTuples (x, y) (x', y') = (x + x', y + y')

hasTree :: WorldMap -> (Int, Int) -> Bool
hasTree map (x, y) = Set.member (x `rem` width map, y) $ treePositions map

treeCountForPath :: WorldMap -> [(Int, Int)] -> Int
treeCountForPath map = length . filter (hasTree map)

treeCountForSlope :: WorldMap -> (Int, Int) -> Int
treeCountForSlope map = treeCountForPath map . generatePath map

loadInput :: String -> IO WorldMap
loadInput fileName = loadRows . BSC.lines <$> BSC.readFile ("src/" ++ fileName)
