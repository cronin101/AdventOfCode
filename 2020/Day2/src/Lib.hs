module Lib
  ( loadInput
  , parseLine
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( ByteString )

-- Example line: "2-5 h: lcwghhkpkxvzkvrmxrv"
parseLine :: ByteString -> (Bool, Bool)
parseLine bs = (testOneResult, testTwoResult)
 where
  -- remainder1: "-5 h: lcwghhkpkxvzkvrmxrv"
  Just (num1      , remainder1) = BSC.readInt bs
  -- remainder2: " h: lcwghhkpkxvzkvrmxrv"
  Just (num2      , remainder2) = BSC.readInt $ BSC.drop 1 remainder1
  -- remainder3: ": lcwghhkpkxvzkvrmxrv"
  Just (targetChar, remainder3) = BSC.uncons $ BSC.drop 1 remainder2
  passwordString                = BSC.drop 2 remainder3
  charCount                     = BSC.count targetChar passwordString
  testOneResult                 = charCount >= num1 && charCount <= num2
  testTwoResult =
    targetCharIsNthChar passwordString targetChar num1
      /= targetCharIsNthChar passwordString targetChar num2

readByteStringNthChar :: ByteString -> Int -> Maybe Char
readByteStringNthChar byteString n
  | n > BSC.length byteString = Nothing
  | otherwise                 = Just $ BSC.index byteString $ n - 1

targetCharIsNthChar :: ByteString -> Char -> Int -> Bool
targetCharIsNthChar byteString targetChar n =
  readByteStringNthChar byteString n == Just targetChar

loadInput :: [Char] -> IO [ByteString]
loadInput fileName = BSC.lines <$> BSC.readFile ("src/" ++ fileName)
