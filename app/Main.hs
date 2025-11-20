module Main where

import Days
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ runDay $ [1 .. 20] ++ [101, 102, 103, 201, 202]
    else runDay (read $ head args)
