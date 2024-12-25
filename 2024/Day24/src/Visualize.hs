{-# LANGUAGE RecordWildCards #-}

module Visualize (toDot) where

import Data
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

toDot :: Input -> String
toDot Input {..} =
  unlines $
    ["digraph Circuit { ranksep=0.01;nodesep=0.01;splines=polyline; size=\"10\";ratio=\"compress\";"]
      ++ [subGraphs (M.elems gatesByOutput')]
      ++ [inputLayer wires']
      ++ [outputLayer wires']
      ++ map renderGate (M.elems gatesByOutput')
      ++ ["}"]
  where
    inputLayer ws = "{ rank=same; " ++ intercalate "; " (S.toAscList $ S.filter (\w -> head w == 'x' || head w == 'y') ws) ++ "}"
    outputLayer ws = "{ rank=same; " ++ intercalate "; " (S.toAscList $ S.filter (\w -> head w == 'z') ws) ++ "}"
    subGraphs g =
      unlines
        [ "subgraph gates_" ++ show operation ++ "{ node [style=filled; color=" ++ c ++ "];" ++ intercalate ";" (map output' gates) ++ "}" | (c, operation) <- zip ["lightblue", "lightgreen", "lightcoral"] [AND, OR, XOR], let gates = filter (\g -> operation' g == operation) g
        ]
    renderGate g =
      unlines
        [ leftInput' g ++ " -> " ++ output' g ++ ";",
          rightInput' g ++ " -> " ++ output' g ++ ";"
        ]