{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    sumInventoriesInPQueue,
    PQ.take,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.PQueue.Prio.Max as PQ

type Inventory = [Int]

sumInventoriesInPQueue :: [Inventory] -> PQ.MaxPQueue Int [Int]
sumInventoriesInPQueue = PQ.fromList . map (\i -> (sum i, i))

-- >>> A.parseOnly parseInventory "1\n2\n3"
-- Right [1,2,3]
parseInventory :: A.Parser Inventory
parseInventory = A.sepBy1 A.decimal A.endOfLine

-- >>> A.parseOnly  parseInventories "1\n2\n\n3\n4"
-- Right [[1,2],[3,4]]
parseInventories :: A.Parser [Inventory]
parseInventories = A.sepBy1 parseInventory (A.count 2 A.endOfLine)

loadInput :: [Char] -> IO [Inventory]
loadInput fileName =
  fromRight [] . A.parseOnly parseInventories <$> BSC.readFile ("src/" ++ fileName)
