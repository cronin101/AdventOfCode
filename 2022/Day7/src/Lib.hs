{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    toTree,
    T.drawTree,
    toINodeSizes,
    removeOrphans,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isPrint)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Tree qualified as T

data INode = Directory String | File Int String
  deriving (Show)

data Command = ChangeDirectory String | LS [INode]
  deriving (Show)

type FolderContents = ([String], [INode])

type FilesInFolderMap = M.Map [String] [INode]

-- >>> A.parseOnly parseCD "$ cd foo"
-- Right (ChangeDirectory {target = "foo"})
parseCD :: A.Parser Command
parseCD = ChangeDirectory . BSC.unpack <$> ("$ cd " *> A.takeWhile1 isPrint)

-- >>> A.parseOnly parseFile "14848514 b.txt"
-- Right (File 14848514 "b.txt")
parseFile :: A.Parser INode
parseFile = do
  size <- A.decimal <* " "
  name <- BSC.unpack <$> A.takeWhile1 isPrint
  return $ File size name

parseDirectory :: A.Parser INode
parseDirectory = do
  name <- BSC.unpack <$> ("dir " *> A.takeWhile1 isPrint)
  return $ Directory name

parseINode :: A.Parser INode
parseINode = parseFile <|> parseDirectory

-- >>> A.parseOnly parseLS "$ ls\n"
-- Right (LS {output = []})
-- >>> A.parseOnly parseLS "$ ls\ndir a\n14848514 b.txt"
-- Right (LS {output = [Directory "dir a",File 14848514 "b.txt"]})
parseLS :: A.Parser Command
parseLS = do
  _ <- "$ ls" <* A.endOfLine
  files <- A.sepBy parseINode A.endOfLine
  return $ LS files

parseCommand :: A.Parser Command
parseCommand = parseCD <|> parseLS

-- >>> A.parseOnly parseTerminal "$ cd foo\n$ cd bar"
-- Right [ChangeDirectory {target = "foo"},ChangeDirectory {target = "bar"}]
parseTerminal :: A.Parser [Command]
parseTerminal = do
  A.sepBy1 parseCommand A.endOfLine

loadInput :: [Char] -> IO (T.Tree INode)
loadInput fileName =
  toTree . toFilesInFoldersMap . toFolderContents [] [] . fromRight [] . A.parseOnly parseTerminal <$> BSC.readFile ("src/" ++ fileName)

toFolderContents :: [String] -> [FolderContents] -> [Command] -> [FolderContents]
toFolderContents _ contents [] = contents
toFolderContents (_ : fs) contents ((ChangeDirectory "..") : cs) = toFolderContents fs contents cs
toFolderContents dir contents ((ChangeDirectory dir') : cs) = toFolderContents (dir' : dir) contents cs
toFolderContents dir contents ((LS content) : cs) = toFolderContents dir ((dir, content) : contents) cs

toFilesInFoldersMap :: [FolderContents] -> FilesInFolderMap
toFilesInFoldersMap = M.fromList . map (\(dirSegments, contents) -> ("/" : drop 1 (reverse dirSegments), contents))

toTree :: FilesInFolderMap -> T.Tree INode
toTree m = T.unfoldTree unfoldDir ([], Directory "/")
  where
    unfoldDir (parentDir, node) = (node, childrenOf parentDir node)
    childrenOf _ (File _ _) = []
    childrenOf parentDir (Directory name) = map (parentDir ++ [name],) $ fromJust ((parentDir ++ [name]) `M.lookup` m)

toINodeSizes :: T.Tree INode -> T.Tree Int
toINodeSizes (T.Node (File size _) _) = T.Node size []
toINodeSizes (T.Node (Directory _) children) = T.Node (sum $ map T.rootLabel childrenSizes) childrenSizes
  where
    childrenSizes = toINodeSizes <$> children

removeOrphans :: T.Tree a -> T.Tree a
removeOrphans (T.Node a children) = T.Node a (map removeOrphans $ filter hasChildren children)
  where
    hasChildren (T.Node _ children') = (not . null) children'
