module Day03 where

import Common
import Data.List (group, nub, sort)
import ECSolution (getInput)

day03 :: String -> IO (Int, Int, Int)
day03 = getInput 3 part1 part2 part3

parseInput :: String -> [Int]
parseInput = sort . commaSeparatedInts

part1 :: String -> Int
part1 = sum . nub . parseInput

part2 :: String -> Int
part2 = sum . take 20 . nub . parseInput

part3 :: String -> Int
part3 = go . parseInput
  where
    go [] = 0
    go xs = 1 + go (concatMap tail $ group xs)
