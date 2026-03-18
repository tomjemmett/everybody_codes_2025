module Main where

import Stories.S1.Quests qualified as S1
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ S1.runQuest [1 .. 3]
    else S1.runQuest (read $ head args)
