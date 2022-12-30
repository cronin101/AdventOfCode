{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    groveSum,
    applyDecryptionKey,
    encrypt,
    encryptN,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM

parseFile :: A.Parser [Int]
parseFile = A.sepBy1 (A.signed A.decimal) A.endOfLine

loadInput :: String -> IO (IM.IntMap [(Int, Int)])
loadInput filename = fromBareList . fromRight [] . A.parseOnly parseFile <$> BSC.readFile ("src/" ++ filename)

toBareList :: IM.IntMap [(Int, Int)] -> [Int]
toBareList = map snd . concatMap snd . IM.toList

fromBareList :: [Int] -> IM.IntMap [(Int, Int)]
fromBareList = IM.fromAscList . zip [0 ..] . zipWith (curry pure) [0 ..]

encryptIndex :: Int -> IM.IntMap [(Int, Int)] -> IM.IntMap [(Int, Int)]
encryptIndex oldIndex m = IM.fromAscList $ zip [0 ..] $ map pure $ concat $ IM.elems m'
  where
    m' = IM.insertWith (flip (++)) newIndex [(oldIndex, value)] without
    currentIndex = fst $ head $ filter ((== oldIndex) . fst . head . snd) $ IM.toList m
    newIndex = (currentIndex + value - 1) `mod` IM.size without
    (_, value) = head $ m IM.! currentIndex
    without = IM.mapKeys (\k -> if k > currentIndex then k - 1 else k) $ IM.delete currentIndex m

encrypt :: IM.IntMap [(Int, Int)] -> IM.IntMap [(Int, Int)]
encrypt input = foldl (flip encryptIndex) input [0 .. length input - 1]

encryptN :: Int -> IM.IntMap [(Int, Int)] -> IM.IntMap [(Int, Int)]
encryptN n input = iterate encrypt input !! max 0 n

groveSum :: IM.IntMap [(Int, Int)] -> Int
groveSum = sumGroveCoords . dropWhile (/= 0) . cycle . toBareList
  where
    sumGroveCoords l =
      let (a, b, c) = (l !! 1000, l !! 2000, l !! 3000)
       in a + b + c

applyDecryptionKey :: IM.IntMap [(Int, Int)] -> IM.IntMap [(Int, Int)]
applyDecryptionKey = IM.map (map (\(i, v) -> (i, v * 811589153)))
