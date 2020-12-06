module Lib
  ( loadInput
  ) where

import           AoCUtils                       ( breakOnBlankLines )
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( unpack
                                                , ByteString
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

parsePersonDeclaration :: ByteString -> Set Char
parsePersonDeclaration = S.fromList . unpack

parseGroupDeclarations :: ByteString -> [Set Char]
parseGroupDeclarations = map parsePersonDeclaration . BSC.lines

loadInput :: [Char] -> IO [[Set Char]]
loadInput fileName =
  map parseGroupDeclarations . breakOnBlankLines <$> BSC.readFile
    ("src/" ++ fileName)

