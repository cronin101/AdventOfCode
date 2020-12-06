module Lib
  ( loadInput
  ) where

import           AoCUtils                       ( breakOnBlankLines )
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString                ( unpack
                                                , ByteString
                                                )
import qualified Data.IntSet                   as S
import           Data.IntSet                    ( IntSet )

-- Storing (ord char) allows the use of IntSet which is superior to Set
parsePersonDeclaration :: ByteString -> IntSet
parsePersonDeclaration = S.fromList . map fromIntegral . unpack

parseGroupDeclarations :: ByteString -> [IntSet]
parseGroupDeclarations = map parsePersonDeclaration . BSC.lines

loadInput :: [Char] -> IO [[IntSet]]
loadInput fileName =
  map parseGroupDeclarations . breakOnBlankLines <$> BSC.readFile
    ("src/" ++ fileName)

