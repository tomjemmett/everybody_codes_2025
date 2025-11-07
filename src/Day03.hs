module Day03 where

import Common
import Data.List (group, nub, sort)
import ECSolution (Solution, getInput, makeSolution, runDay)

day03 :: String -> IO (Int, Int, Int)
day03 = getInput 3 part1 part2 part3

part1 :: String -> Int
part1 = sum . nub . sort . commaSeparatedInts

part2 :: String -> Int
part2 = sum . take 20 . nub . sort . commaSeparatedInts

part3 :: String -> Int
part3 = go . sort . commaSeparatedInts
  where
    go [] = 0
    go xs = 1 + go (concatMap tail $ group xs)
