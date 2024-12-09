{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where

import Data.IntMap qualified as IM

data FileBlock where
  FileBlock :: {blockId :: Int} -> FileBlock
  deriving (Show, Eq)

type Block = Maybe FileBlock

data FileSegment where
  FileSegment :: {fileSize :: Int, fileId :: Int} -> FileSegment
  deriving (Show)

data FreeSegment where
  FreeSegment :: {freeSize :: Int} -> FreeSegment
  deriving (Show)

data FileSystem = FileSystem
  { freeSegments :: IM.IntMap FreeSegment,
    fileSegments :: IM.IntMap FileSegment
  }

-- Linear merge of segments to print
instance Show FileSystem where
  show (FileSystem freeSegments fileSegments)
    | IM.null freeSegments && IM.null fileSegments = ""
    | IM.null freeSegments = concatMap (\(FileSegment size fId) -> concat $ replicate size (show fId)) $ IM.elems fileSegments
    | IM.null fileSegments = concatMap (\(FreeSegment size) -> concat $ replicate size ".") $ IM.elems freeSegments
    | otherwise = next ++ show (FileSystem freeSegments' fileSegments')
    where
      (firstFreeSegment, FreeSegment freeSegmentLength) = IM.findMin freeSegments
      (firstFileSegment, FileSegment fileSegmentLength fId) = IM.findMin fileSegments
      next =
        concat $
          if firstFreeSegment < firstFileSegment
            then replicate freeSegmentLength "."
            else replicate fileSegmentLength (show fId)
      freeSegments' = if firstFreeSegment < firstFileSegment then IM.deleteMin freeSegments else freeSegments
      fileSegments' = if firstFreeSegment < firstFileSegment then fileSegments else IM.deleteMin fileSegments

data Span = FileSpan Int Int | EmptySpan Int
  deriving (Show)
