module Main where

import Stories.S3.Quests qualified as S3
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ S3.runQuest [1 .. 3]
    else S3.runQuest (read $ head args)
