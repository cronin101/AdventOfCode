{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                            ( loadInput
                                                , calculateTicketScanningErrorRate
                                                , solveTicket
                                                )

import           Data.ByteString.Char8          ( stripPrefix )
import           Data.Maybe                     ( mapMaybe )

main :: IO ()
main = do
  input <- loadInput "input16.txt"
  let errorRate = calculateTicketScanningErrorRate input
  print errorRate

  let solvedTicket = solveTicket input
  let departureColumns = mapMaybe
        (\column@(name, _) -> column <$ stripPrefix "departure" name)
        solvedTicket

  print $ product $ map snd departureColumns

