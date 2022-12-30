module Main (main) where

import Lib (applyDecryptionKey, encrypt, encryptN, groveSum, loadInput)

main :: IO ()
main = do
  input <- loadInput "input.txt"
  print $ groveSum $ encrypt input
  let decryptedInput = applyDecryptionKey input
  print $ groveSum $ encryptN 10 decryptedInput
