module Main where

import Events.Y2025.Quests qualified as Y2025
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ Y2025.runQuest [1 .. 20]
    else Y2025.runQuest (read $ head args)
