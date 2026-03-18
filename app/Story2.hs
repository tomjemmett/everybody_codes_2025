module Main where

import Stories.S2.Quests qualified as S2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ S2.runQuest [1 .. 3]
    else S2.runQuest (read $ head args)
