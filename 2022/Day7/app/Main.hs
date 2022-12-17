module Main (main) where

import Lib (drawTree, loadInput, removeOrphans, toINodeSizes)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  putStr $ drawTree (show <$> input)
  let folderSizes = foldr (:) [] $ (removeOrphans . toINodeSizes) input
  let smallFolderSizes = filter (<= 100000) folderSizes
  print $ sum smallFolderSizes

  let totalSpace = 70000000
  let desiredSpace = 30000000
  let (totalUsed : subFolderSizes) = folderSizes
  let currentSpace = totalSpace - totalUsed
  let folderToDelete = minimum $ filter (>= (desiredSpace - currentSpace)) subFolderSizes

  print folderToDelete
