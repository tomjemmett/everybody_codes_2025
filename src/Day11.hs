module Day11 where

import Common
import Control.Arrow (Arrow (second), (>>>))
import ECSolution (getInput)

day11 :: String -> IO (Int, Int, Int)
day11 = getInput 11 part1 part2 part3

part1 :: String -> Int
part1 = run >>> (!! 10) >>> zipWith (*) [1 ..] >>> sum

part2 :: String -> Int
part2 = run >>> length >>> pred

part3 :: String -> Int
part3 input = sum r
  where
    xs = parseInput input
    m = sum xs `div` length xs
    r = [m - x | x <- xs, x < m]

run :: String -> [[Int]]
run = parseInput >>> goFirst
  where
    goFirst xs =
      let xs' = phase (<=) pred succ xs
       in if xs == xs' then goSecond xs else xs : goFirst xs'
    goSecond xs =
      let xs' = phase (>=) succ pred xs
       in if xs == xs' then [xs] else xs : goSecond xs'
    phase :: (Int -> Int -> Bool) -> (Int -> Int) -> (Int -> Int) -> [Int] -> [Int]
    phase comp nx ny = go
      where
        go [x] = [x]
        go (x : y : xs)
          | comp x y = x : go (y : xs)
          | otherwise = nx x : go (ny y : xs)

parseInput :: String -> [Int]
parseInput = map read . lines
