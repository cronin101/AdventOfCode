{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.List (intercalate)

-- >>> A.parseOnly (parseFileSegment 1) "2"
-- Right (FileSegment {fileSize = 2, fileId = 1})
parseFileSegment :: Int -> A.Parser FileSegment
parseFileSegment filesSeen = (`FileSegment` filesSeen) . read . pure <$> A.digit

-- >>> A.parseOnly parseFreeSegment "2"
-- Right (FreeSegment {freeSize = 2})
parseFreeSegment :: A.Parser FreeSegment
parseFreeSegment = FreeSegment . read . pure <$> A.digit

-- >>> A.parseOnly parseDiskMap "2333133121414131402"
-- Right 00...111...2...333.44.5555.6666.777.888899
parseDiskMap :: A.Parser FileSystem
parseDiskMap = parseDiskMap' True 0 $ FileSystem IM.empty IM.empty
  where
    parseDiskMap' fileComesNext idx fs@(FileSystem free files) = do
      let nextParser =
            if fileComesNext
              then (\nextFile -> (FileSystem free $ IM.insert idx nextFile files, fileSize nextFile)) <$> parseFileSegment (IM.size files)
              else (\nextFree -> (FileSystem (IM.insert idx nextFree free) files, freeSize nextFree)) <$> parseFreeSegment
      next <- A.choice [Just <$> nextParser, Nothing <$ A.endOfInput]
      case next of
        Just (fs', offset) -> parseDiskMap' (not fileComesNext) (idx + offset) fs'
        Nothing -> pure fs

-- >>> fractureSegments <$> loadInput "example.txt"
-- 00...111...2...333.44.5555.6666.777.888899
fractureSegments :: FileSystem -> FileSystem
fractureSegments (FileSystem freeSegments fileSegments) = FileSystem freeSegments' fileSegments'
  where
    freeSegments' = IM.fromList $ concatMap (\(idx, FreeSegment size) -> map (,FreeSegment 1) [idx .. idx + size - 1]) $ IM.toList freeSegments
    fileSegments' = IM.fromList $ concatMap (\(idx, FileSegment size index) -> map (,FileSegment 1 index) [idx .. idx + size - 1]) $ IM.toList fileSegments

isCompacted :: FileSystem -> Bool
isCompacted (FileSystem free files) = fst (IM.findMin free) > fst (IM.findMax files)

-- >>>   step . fractureSegments <$> loadInput "example.txt"
-- 009..111...2...333.44.5555.6666.777.88889.
step :: FileSystem -> FileSystem
step fs@(FileSystem free files)
  | isCompacted fs = fs
  | otherwise = FileSystem free'' files''
  where
    ((freeIdx, lowestFree@(FreeSegment 1)), free') = IM.deleteFindMin free
    ((fileIdx, highestFile@(FileSegment 1 _)), files') = IM.deleteFindMax files
    files'' = IM.insert freeIdx highestFile files'
    free'' = IM.insert fileIdx lowestFree free'

-- >>> compact <$> loadInput "example.txt"
-- 0099811188827773336446555566..............
compact :: FileSystem -> FileSystem
compact = head . dropWhile (not . isCompacted) . iterate step . fractureSegments

loadInput :: [Char] -> IO FileSystem
loadInput = (fromRight (FileSystem IM.empty IM.empty) . A.parseOnly parseDiskMap <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 1928
part1 :: FileSystem -> Int
part1 = checksum . compact

defragmentSteps :: FileSystem -> [FileSystem]
defragmentSteps fs = scanl defragment1 fs (reverse $ IM.toList $ fileSegments fs)
  where
    defragment1 fs@(FileSystem free files) (fileIndex, file@(FileSegment fileSize _)) = case IM.lookupMin (IM.filter ((>= fileSize) . freeSize) $ IM.filterWithKey (const . (< fileIndex)) free) of
      Just (freeIndex, freeSegment) ->
        let leftoverFree = freeSize freeSegment - fileSize
            free' = (if leftoverFree > 0 then IM.insert (freeIndex + fileSize) (FreeSegment leftoverFree) else id) $ IM.insert fileIndex (FreeSegment fileSize) $ IM.delete freeIndex free
            files' = IM.insert freeIndex file $ IM.delete fileIndex files
         in FileSystem free' files'
      _ -> fs

-- >>> defragment <$> loadInput "example.txt"
-- 00992111777.44.333....5555.6666.....8888..
defragment :: FileSystem -> FileSystem
defragment = last . defragmentSteps

checksum :: FileSystem -> Int
checksum = sum . map segmentValue . IM.toList . fileSegments
  where
    segmentValue (idx, FileSegment size fId) = sum $ map (fId *) [idx .. idx + size - 1]

-- >>> part2 <$> loadInput "example.txt"
-- 2858
part2 :: FileSystem -> Int
part2 = checksum . defragment
