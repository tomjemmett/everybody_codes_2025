module Main where

import Events.Y2024.Quests qualified as Y2024
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ Y2024.runQuest [1 .. 20]
    else Y2024.runQuest (read $ head args)
