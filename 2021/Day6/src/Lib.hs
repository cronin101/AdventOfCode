{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    afterDays,
    fishCount,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.IntMap as M

type CountsByAge = M.IntMap Int

-- >>> A.parseOnly parseAges "3,4,3,1,2"
-- Right [3,4,3,1,2]
parseAges :: A.Parser [Int]
parseAges = A.sepBy1 A.decimal ","

-- >>> loadInput "example.txt"
-- fromList [(1,1),(2,1),(3,2),(4,1)]
loadInput :: Num a => [Char] -> IO (M.IntMap a)
loadInput fileName =
  M.unionsWith (+) . map (uncurry M.singleton) . flip zip (repeat 1) . fromRight []
    . A.parseOnly parseAges
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> nextDay $ M.fromList [(0,1),(1,1),(2,2),(3,1)]
-- fromList [(0,1),(1,2),(2,1),(6,1),(8,1)]
nextDay :: CountsByAge -> CountsByAge
nextDay counts = M.delete (-1) $ M.unionWith (+) newEntries $ M.mapKeys (+ (-1)) counts
  where
    newEntries = case M.lookup 0 counts of
      Just newCount -> M.union (M.singleton 6 newCount) (M.singleton 8 newCount)
      Nothing -> M.empty

afterDays :: Int -> CountsByAge -> CountsByAge
afterDays n = (!! n) . iterate nextDay

fishCount :: CountsByAge -> Int
fishCount = sum . M.elems

-- >>> fishCount $ afterDays 80 $ M.fromList [(1,1),(2,1),(3,2),(4,1)]
-- 5934

-- >>> fishCount $ afterDays 256 $ M.fromList [(1,1),(2,1),(3,2),(4,1)]
-- 26984457539
