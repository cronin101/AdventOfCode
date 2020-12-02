module Lib
  ( loadInput
  , parseLine
  , isValid1
  , isValid2
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( ByteString )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet

data ParsedLine = ParsedLine Int Int IntSet
  deriving Show

isValid1 :: ParsedLine -> Bool
isValid1 (ParsedLine minimumcount maximumCount indices) =
  charCount >= minimumcount && charCount <= maximumCount
  where charCount = IntSet.size indices

isValid2 :: ParsedLine -> Bool
isValid2 (ParsedLine index1 index2 indices) =
  IntSet.member (index1 - 1) indices /= IntSet.member (index2 - 1) indices

-- Example line: "2-5 h: lcwghhkpkxvzkvrmxrv"
parseLine :: ByteString -> ParsedLine
parseLine bs = ParsedLine num1 num2 indices
 where
  -- remainder1: "-5 h: lcwghhkpkxvzkvrmxrv"
  Just (num1      , remainder1) = BSC.readInt bs
  -- remainder2: " h: lcwghhkpkxvzkvrmxrv"
  Just (num2      , remainder2) = BSC.readInt $ BSC.drop 1 remainder1
  -- remainder3: ": lcwghhkpkxvzkvrmxrv"
  Just (targetChar, remainder3) = BSC.uncons $ BSC.drop 1 remainder2
  passwordString                = BSC.drop 2 remainder3
  indices =
    IntSet.fromDistinctAscList $ BSC.elemIndices targetChar passwordString

loadInput :: [Char] -> IO [ByteString]
loadInput fileName = BSC.lines <$> BSC.readFile ("src/" ++ fileName)
