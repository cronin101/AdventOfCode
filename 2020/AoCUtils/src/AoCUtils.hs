module AoCUtils
  ( breakOnBlankLines
  , byteStringWithPrefixParser
  ) where

import           Data.ByteString.Char8          ( ByteString
                                                , pack
                                                )
import qualified Data.ByteString.Char8         as BSC
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser
                                                , string
                                                , takeByteString
                                                )


breakOnBlankLines :: ByteString -> [ByteString]
breakOnBlankLines bs
  | BSC.null beforeBlankLine = []
  | otherwise = beforeBlankLine : breakOnBlankLines afterBlankLine
 where
  afterBlankLine          = BSC.drop (BSC.length blankLine) rest
  (beforeBlankLine, rest) = BSC.breakSubstring blankLine bs
  blankLine               = BSC.pack "\n\n"

byteStringWithPrefixParser
  :: String -> (ByteString -> Bool) -> (ByteString -> a) -> Parser a
byteStringWithPrefixParser prefix predicate f = do
  string $ pack prefix
  byteString <- takeByteString
  if predicate byteString then return $ f byteString else fail "Predicate"
