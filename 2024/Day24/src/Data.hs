{-# LANGUAGE GADTs #-}

module Data where

import qualified Data.Map as M
import qualified Data.Set as S

data Operation = AND | OR | XOR deriving (Show, Eq, Ord)

data Gate where
  Gate ::
    { leftInput' :: String,
      operation' :: Operation,
      rightInput' :: String,
      output' :: String
    } ->
    Gate
  deriving (Show, Eq, Ord)

data Input = Input
  { initialValues' :: M.Map String Int,
    gatesByOutput' :: M.Map String Gate,
    wires' :: S.Set String
  }
  deriving (Show, Eq, Ord)