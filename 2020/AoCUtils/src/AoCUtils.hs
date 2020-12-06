module AoCUtils
  ( breakOnBlankLines
  ) where

import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BSC

breakOnBlankLines :: ByteString -> [ByteString]
breakOnBlankLines bs
  | BSC.null beforeBlankLine = []
  | otherwise = beforeBlankLine : breakOnBlankLines afterBlankLine
 where
  afterBlankLine          = BSC.drop (BSC.length blankLine) rest
  (beforeBlankLine, rest) = BSC.breakSubstring blankLine bs
  blankLine               = BSC.pack "\n\n"
