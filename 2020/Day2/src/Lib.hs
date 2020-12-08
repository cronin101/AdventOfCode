module Lib
  ( loadInput
  , evaluateLine
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( ByteString )
import           Data.Attoparsec.ByteString.Char8
                                                ( Parser )
import           Data.Attoparsec.ByteString.Char8
                                               as P
                                                ( parseOnly
                                                , letter_ascii
                                                , space
                                                , char
                                                , takeByteString
                                                , takeWhile
                                                , letter_ascii
                                                , isDigit
                                                )
import           Data.Maybe                     ( fromJust )

-- More efficient than parsec but less elegant
-- Example line: "2-5 h: lcwghhkpkxvzkvrmxrv"
parseLine' :: ByteString -> (Int, Int, Char, ByteString)
parseLine' bs = (num1, num2, targetChar, passwordString)
 where
  -- remainder1: "-5 h: lcwghhkpkxvzkvrmxrv"
  Just (num1      , remainder1) = BSC.readInt bs
  -- remainder2: " h: lcwghhkpkxvzkvrmxrv"
  Just (num2      , remainder2) = BSC.readInt $ BSC.drop 1 remainder1
  -- remainder3: ": lcwghhkpkxvzkvrmxrv"
  Just (targetChar, remainder3) = BSC.uncons $ BSC.drop 1 remainder2
  passwordString                = BSC.drop 2 remainder3

parseLine :: ByteString -> (Int, Int, Char, ByteString)
parseLine bs = getLine $ parseOnly lineParser bs
  where getLine (Right line) = line

lineParser :: Parser (Int, Int, Char, ByteString)
lineParser = do
  let intFromBS = fst . fromJust . BSC.readInt
  low <- intFromBS <$> P.takeWhile P.isDigit
  P.char '-'
  high <- intFromBS <$> P.takeWhile P.isDigit
  P.space
  targetChar <- P.letter_ascii
  P.char ':'
  P.space
  password <- P.takeByteString
  return (low, high, targetChar, password)

evaluateLine :: ByteString -> (Bool, Bool)
evaluateLine bs = (testOneResult, testTwoResult)
 where
  (num1, num2, targetChar, passwordString) = parseLine bs
  charCount     = BSC.count targetChar passwordString
  testOneResult = charCount >= num1 && charCount <= num2
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
