module Days
  ( day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12,
    day13,
    day14,
    day15,
    day16,
    day17,
    day18,
    day19,
    day20,
    runDay,
  )
where

import Common
import Control.Monad (when)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import Day20 (day20)
import ECSolution (makeSolution, showDay)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)

days =
  [ fmap makeSolution . day01,
    fmap makeSolution . day02,
    fmap makeSolution . day03,
    fmap makeSolution . day04,
    fmap makeSolution . day05,
    fmap makeSolution . day06,
    fmap makeSolution . day07,
    fmap makeSolution . day08,
    fmap makeSolution . day09,
    fmap makeSolution . day10,
    fmap makeSolution . day11,
    fmap makeSolution . day12,
    fmap makeSolution . day13,
    fmap makeSolution . day14,
    fmap makeSolution . day15,
    fmap makeSolution . day16,
    fmap makeSolution . day17,
    fmap makeSolution . day18,
    fmap makeSolution . day19,
    fmap makeSolution . day20
  ]

runDay :: Int -> IO ()
runDay day = do
  let d = days !! pred day
  timeIt do
    putStrLn $ replicate 80 '-'
    putStrLn $ "Day: " ++ show day
    putStrLn ""
    putStrLn "Sample:"
    d "sample" >>= showDay
    putStrLn ""
    putStrLn "Actual:"
    d "actual" >>= showDay
    putStrLn ""
