{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    caveTraversals,
    allTraversalsWithRevisit,
  )
where

import Control.Arrow (Arrow (second))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isLower, isUpper)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Tuple (swap)

type Tunnel = (String, String)

type TunnelMap = M.Map String [String]

parseCaveName :: A.Parser String
parseCaveName = BSC.unpack <$> A.takeWhile1 A.isAlpha_ascii

-- >>> A.parseOnly parseTunnel "start-A"
-- Right ("start","A")
parseTunnel :: A.Parser Tunnel
parseTunnel = do
  left <- parseCaveName
  "-"
  right <- parseCaveName
  return (left, right)

parseTunnels :: A.Parser [Tunnel]
parseTunnels = A.sepBy1 parseTunnel A.endOfLine

-- >>> tunnelToBidirectionalMap ("start","A")
-- fromList [("A",["start"]),("start",["A"])]
tunnelToBidirectionalMap :: Tunnel -> TunnelMap
tunnelToBidirectionalMap tunnel = M.fromList $ map (second pure) [tunnel, swap tunnel]

-- >>> loadInput "smallExample.txt"
-- fromList [("A",["start","c","b","end"]),("b",["start","A","d","end"]),("c",["A"]),("d",["b"]),("end",["A","b"]),("start",["A","b"])]
loadInput fileName =
  M.unionsWith (++)
    . map tunnelToBidirectionalMap
    . fromRight []
    . A.parseOnly parseTunnels
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> allPathsFrom "start" $ M.fromList [("A",["start","c","b","end"]),("b",["start","A","d","end"]),("c",["A"]),("d",["b"]),("end",["A","b"]),("start",["A","b"])]
-- [["start"],["start","A"],["start","A","c"],["start","A","c","A"],["start","A","c","A","b"],["start","A","c","A","b","A"],["start","A","c","A","b","A","end"],["start","A","c","A","b","A","end","A"],["start","A","c","A","b","d"],["start","A","c","A","b","end"],["start","A","c","A","b","end","A"],["start","A","c","A","end"],["start","A","c","A","end","A"],["start","A","c","A","end","A","b"],["start","A","c","A","end","A","b","A"],["start","A","c","A","end","A","b","d"],["start","A","c","A","end","b"],["start","A","c","A","end","b","A"],["start","A","c","A","end","b","d"],["start","A","b"],["start","A","b","A"],["start","A","b","A","c"],["start","A","b","A","c","A"],["start","A","b","A","c","A","end"],["start","A","b","A","c","A","end","A"],["start","A","b","A","end"],["start","A","b","A","end","A"],["start","A","b","A","end","A","c"],["start","A","b","A","end","A","c","A"],["start","A","b","d"],["start","A","b","end"],["start","A","b","end","A"],["start","A","b","end","A","c"],["start","A","b","end","A","c","A"],["start","A","end"],["start","A","end","A"],["start","A","end","A","c"],["start","A","end","A","c","A"],["start","A","end","A","c","A","b"],["start","A","end","A","c","A","b","A"],["start","A","end","A","c","A","b","d"],["start","A","end","A","b"],["start","A","end","A","b","A"],["start","A","end","A","b","A","c"],["start","A","end","A","b","A","c","A"],["start","A","end","A","b","d"],["start","A","end","b"],["start","A","end","b","A"],["start","A","end","b","A","c"],["start","A","end","b","A","c","A"],["start","A","end","b","d"],["start","b"],["start","b","A"],["start","b","A","c"],["start","b","A","c","A"],["start","b","A","c","A","end"],["start","b","A","c","A","end","A"],["start","b","A","end"],["start","b","A","end","A"],["start","b","A","end","A","c"],["start","b","A","end","A","c","A"],["start","b","d"],["start","b","end"],["start","b","end","A"],["start","b","end","A","c"],["start","b","end","A","c","A"]]
allPathsFrom :: String -> TunnelMap -> [[String]]
allPathsFrom start tunnelMap = pathsRec tunnelMap start [start] (S.singleton start)
  where
    pathsRec :: TunnelMap -> String -> [String] -> S.Set String -> [[String]]
    pathsRec m point path visited = path : concatMap (\option -> pathsRec m option (path ++ [option]) (S.insert option visited)) options
      where
        options = filter (\option -> isUpper (head option) || not (S.member option visited)) $ fromMaybe [] $ M.lookup point m

-- >>> allTraversals $ M.fromList [("A",["start","c","b","end"]),("b",["start","A","d","end"]),("c",["A"]),("d",["b"]),("end",["A","b"]),("start",["A","b"])]
-- [["start","A","c","A","b","A","end"],["start","A","c","A","b","end"],["start","A","c","A","end"],["start","A","b","A","c","A","end"],["start","A","b","A","end"],["start","A","b","end"],["start","A","end"],["start","b","A","c","A","end"],["start","b","A","end"],["start","b","end"]]
allTraversals :: TunnelMap -> [[String]]
allTraversals = filter ((== "end") . last) . allPathsFrom "start"

-- >>> caveTraversals <$> loadInput "smallExample.txt"
-- [["start","A","c","A","b","A","end"],["start","A","c","A","b","end"],["start","A","c","A","end"],["start","A","b","A","c","A","end"],["start","A","b","A","end"],["start","A","b","end"],["start","b","A","c","A","end"],["start","b","A","end"],["start","b","end"]]
caveTraversals :: TunnelMap -> [[String]]
caveTraversals = filter (any (isLower . head) . drop 1 . init) . allTraversals

-- >>> length . allTraversals <$> loadInput "smallExample.txt"
-- 10

-- >>> length . allTraversals <$> loadInput "mediumExample.txt"
-- 19

-- >>> length . allTraversals <$> loadInput "largeExample.txt"
-- 226

-- >>> length . caveTraversals <$> loadInput "input.txt"
-- 4912

-- >>> allPathsWithRevisitFrom "start" $ M.fromList [("A",["start","c","b","end"]),("b",["start","A","d","end"]),("c",["A"]),("d",["b"]),("end",["A","b"]),("start",["A","b"])]
-- [["start"],["start","A"],["start","A","c"],["start","A","c","A"],["start","A","c","A","b"],["start","A","c","A","b","A"],["start","A","c","A","b","A","end"],["start","A","c","A","b","A","end","A"],["start","A","c","A","b","d"],["start","A","c","A","b","end"],["start","A","c","A","b","end","A"],["start","A","c","A","end"],["start","A","c","A","end","A"],["start","A","c","A","end","A","b"],["start","A","c","A","end","A","b","A"],["start","A","c","A","end","A","b","d"],["start","A","c","A","end","b"],["start","A","c","A","end","b","A"],["start","A","c","A","end","b","d"],["start","A","b"],["start","A","b","A"],["start","A","b","A","c"],["start","A","b","A","c","A"],["start","A","b","A","c","A","end"],["start","A","b","A","c","A","end","A"],["start","A","b","A","end"],["start","A","b","A","end","A"],["start","A","b","A","end","A","c"],["start","A","b","A","end","A","c","A"],["start","A","b","d"],["start","A","b","end"],["start","A","b","end","A"],["start","A","b","end","A","c"],["start","A","b","end","A","c","A"],["start","A","end"],["start","A","end","A"],["start","A","end","A","c"],["start","A","end","A","c","A"],["start","A","end","A","c","A","b"],["start","A","end","A","c","A","b","A"],["start","A","end","A","c","A","b","d"],["start","A","end","A","b"],["start","A","end","A","b","A"],["start","A","end","A","b","A","c"],["start","A","end","A","b","A","c","A"],["start","A","end","A","b","d"],["start","A","end","b"],["start","A","end","b","A"],["start","A","end","b","A","c"],["start","A","end","b","A","c","A"],["start","A","end","b","d"],["start","b"],["start","b","A"],["start","b","A","c"],["start","b","A","c","A"],["start","b","A","c","A","end"],["start","b","A","c","A","end","A"],["start","b","A","end"],["start","b","A","end","A"],["start","b","A","end","A","c"],["start","b","A","end","A","c","A"],["start","b","d"],["start","b","end"],["start","b","end","A"],["start","b","end","A","c"],["start","b","end","A","c","A"]]
allPathsWithRevisitFrom :: String -> TunnelMap -> [[String]]
allPathsWithRevisitFrom start tunnelMap = pathsRec tunnelMap start [start] (S.singleton start) Nothing
  where
    pathsRec :: TunnelMap -> String -> [String] -> S.Set String -> Maybe String -> [[String]]
    pathsRec m point path visited revisit = path : concatMap (\option -> pathsRec m option (path ++ [option]) (S.insert option visited) (nextRevisit visited option)) options
      where
        nextRevisit visited option = if isNothing revisit && S.member option visited && isLower (head option) then Just option else revisit
        options = filter (\option -> isUpper (head option) || not (S.member option visited) || (isNothing revisit && not (isTerminal option))) $ fromMaybe [] $ M.lookup point m
        isTerminal option = option == "start" || option == "end"

-- >>> allTraversalsWithRevisit <$> loadInput "smallExample.txt"
-- [["start","A","c","A","b","A","end"],["start","A","c","A","b","end"],["start","A","c","A","end"],["start","A","b","A","c","A","end"],["start","A","b","A","end"],["start","A","b","d","b","A","c","A","end"],["start","A","b","d","b","A","end"],["start","A","b","d","b","end"],["start","A","b","end"],["start","A","end"],["start","b","A","c","A","end"],["start","b","A","b","A","c","A","end"],["start","b","A","b","A","end"],["start","b","A","b","end"],["start","b","A","end"],["start","b","d","b","A","c","A","end"],["start","b","d","b","A","end"],["start","b","d","b","end"],["start","b","end"]]
allTraversalsWithRevisit :: TunnelMap -> [[String]]
allTraversalsWithRevisit = filter ((== "end") . last) . allPathsWithRevisitFrom "start"

-- >>> length . allTraversalsWithRevisit <$> loadInput "smallExample.txt"
-- 36

-- >>> length . allTraversalsWithRevisit <$> loadInput "mediumExample.txt"
-- 103

-- >>> length . allTraversalsWithRevisit <$> loadInput "largeExample.txt"
-- 3509

-- >>> length . allTraversalsWithRevisit <$> loadInput "input.txt"
-- 150004
