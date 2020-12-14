{-# LANGUAGE OverloadedStrings, Strict #-}

module Lib
  ( loadInput
  , scanStates
  , sumData
  , MachineVersion(VersionOne, VersionTwo)
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Attoparsec.ByteString.Char8
                                                ( parseOnly
                                                , endOfLine
                                                , sepBy1
                                                , choice
                                                , isDigit
                                                , takeWhile1
                                                , isSpace
                                                , string
                                                , Parser
                                                )
import           Data.Word                      ( Word64 )
import qualified Data.Bits                     as B
import           Data.Either                    ( fromRight )
import qualified Data.IntMap.Strict            as M
import           Data.List                      ( foldl'
                                                , subsequences
                                                , scanl'
                                                )

type SubMask = Word64
type Idx = Word64
type Value = Word64

data Mask = Mask
  { and :: SubMask
  , or  :: SubMask
  }
  deriving Show

data Instruction = SetMask Mask | Assign Idx Value
    deriving (Show)

data MachineState = MachineState MachineVersion Mask (M.IntMap Value)
  deriving Show

data MachineVersion = VersionOne | VersionTwo
    deriving (Show)

trueMask :: SubMask
trueMask = (2 ^ 36) - 1

falseMask :: SubMask
falseMask = 0

initialState :: MachineVersion -> MachineState
initialState version = MachineState version (Mask trueMask falseMask) M.empty

-- Make a mask where bits are equal to target char, e.g. x1x0 -> 1 -> 0100, x1x0 -> 0 -> 0001
elemSubMask :: Char -> BSC.ByteString -> SubMask
elemSubMask char bytestring =
  sum . map ((2 ^) . (35 -)) $ BSC.elemIndices char bytestring

parseMask :: Parser Instruction
parseMask = do
  string "mask = "
  maskString <- takeWhile1 (not . isSpace)
  let andMask = trueMask `B.xor` elemSubMask '0' maskString
  let orMask  = falseMask B..|. elemSubMask '1' maskString
  return $ SetMask $ Mask andMask orMask

parseAssignment :: Parser Instruction
parseAssignment = do
  string "mem["
  Just (idx, _) <- BSC.readInt <$> takeWhile1 isDigit
  string "] = "
  Just (value, _) <- BSC.readInt <$> takeWhile1 isDigit
  return $ Assign (fromIntegral idx) (fromIntegral value)

parseInstruction :: Parser Instruction
parseInstruction = choice [parseMask, parseAssignment]

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy1 parseInstruction endOfLine

step :: MachineState -> Instruction -> MachineState
step (MachineState version _ values) (SetMask mask) =
  MachineState version mask values
step (MachineState version mask values) assignment =
  MachineState version mask
    $ (case version of
        VersionOne -> assignWithMask
        VersionTwo -> assignMany
      )
        assignment
        mask
        values

assignWithMask :: Instruction -> Mask -> M.IntMap Value -> M.IntMap Value
assignWithMask (Assign idx value) (Mask and or) = M.insert (fromIntegral idx)
                                                           maskedValue
  where maskedValue = value B..&. and B..|. or

assignMany :: Instruction -> Mask -> M.IntMap Value -> M.IntMap Value
assignMany (Assign idx value) (Mask and or) machineData = foldl'
  (\map i -> M.insert (fromIntegral i) value map)
  machineData
  floatingAddresses
 where
  floatingMask         = trueMask `B.xor` ((trueMask `B.xor` and) `B.xor` or)
  floatingValues       = map (2 ^) $ filter (floatingMask `B.testBit`) [0 .. 36]
  floatingCombinations = map sum $ subsequences floatingValues
  floatingAddresses    = map (baseAddress B..|.) floatingCombinations
  baseAddress          = (idx B..|. or) B..&. (trueMask `B.xor` floatingMask)

scanStates :: MachineVersion -> [Instruction] -> [MachineState]
scanStates version = scanl' step (initialState version)

sumData :: MachineState -> Word64
sumData (MachineState _ _ values) = M.foldl (+) 0 values

loadInput :: String -> IO [Instruction]
loadInput fileName =
  fromRight [] . parseOnly parseInstructions <$> BSC.readFile
    ("src/" ++ fileName)
