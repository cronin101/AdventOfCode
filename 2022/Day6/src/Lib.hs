{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( loadInput,
    advanceToMarker,
    processedReversed
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BSC
import           Data.Char                        (isAlpha)
import           Data.Either                      (fromRight)
import           Data.List                        (nub)

data DataStream = DataStream { processedReversed :: String, remaining :: String }
    deriving (Show)

-- >>> A.parseOnly parseDataStream "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
-- Right (DataStream {processedReversed = "", remaining = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"})
parseDataStream :: A.Parser DataStream
parseDataStream = DataStream "" . BSC.unpack <$> A.takeWhile1 isAlpha

-- >>> advanceToMarker 4 (DataStream "" "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
-- DataStream {processedReversed = "rfqzzwfzfcz", remaining = "ljwzlrfnpqdbhtmscgvjw"}
-- >>> advanceToMarker 14 (DataStream "" "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
-- DataStream {processedReversed = "", remaining = ""}
advanceToMarker :: Int -> DataStream -> DataStream
advanceToMarker _ ds@(DataStream _ []) = ds
advanceToMarker markerLength (DataStream pr rem@(r:rs))
    | ((== markerLength) . length) marker = DataStream (marker ++ pr) (drop markerLength rem)
    | otherwise = advanceToMarker markerLength $ DataStream (r:pr) rs
        where marker = reverse . nub . take markerLength $ rem

loadInput :: [Char] -> IO DataStream
loadInput fileName =
  fromRight (DataStream "" "") . A.parseOnly parseDataStream <$> BSC.readFile ("src/" ++ fileName)
