module Main where

import           Lib                            ( loadInput
                                                , detectLoopStart
                                                , executeProgram
                                                , scanProgram
                                                , indexFromState
                                                , ProgramState
                                                  ( Running
                                                  , Complete
                                                  )
                                                , findIndexToFix
                                                , toggleAtIndex
                                                )
import           Data.List                      ( find )

main :: IO ()
main = do
  program <- loadInput "input8.txt"
  let Just loopStartIndex = detectLoopStart program
  let programExecution    = scanProgram program

  -- Find state when the cycle index is reached for the second time
  let Just (Running (_, cycleAccumulator)) =
        find ((== loopStartIndex) . indexFromState) $ drop 1 $ dropWhile
          ((/= loopStartIndex) . indexFromState)
          programExecution

  print cycleAccumulator

  let Just indexToFix = findIndexToFix program
  let (Complete finalAccumulator) =
        executeProgram (toggleAtIndex program indexToFix)
  print finalAccumulator




