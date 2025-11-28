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
    s1q1,
    s1q2,
    s1q3,
    s2q1,
    s2q2,
    s2q3,
    downloadNotes,
  )
where

import Common
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
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
import ECSolution
import GetInputs (downloadNotes, submitAnswer)
import Stories.S1.Quest01 (s1q1)
import Stories.S1.Quest02 (s1q2)
import Stories.S1.Quest03 (s1q3)
import Stories.S2.Quest01 (s2q1)
import Stories.S2.Quest02 (s2q2)
import Stories.S2.Quest03 (s2q3)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)
import Text.Printf (printf)

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
  let d = case day of
        101 -> fmap makeSolution . s1q1
        102 -> fmap makeSolution . s1q2
        103 -> fmap makeSolution . s1q3
        201 -> fmap makeSolution . s2q1
        202 -> fmap makeSolution . s2q2
        203 -> fmap makeSolution . s2q3
        _ -> days !! pred day
  contents <- readFile (createPath "sample" day 1)

  when (contents /= "") $ timeIt do
    putStrLn $ replicate 80 '-'
    putStrLn $ "Day: " ++ show day
    putStrLn ""
    putStrLn "Sample:"
    d "sample" >>= showDay
    putStrLn ""
    putStrLn "Actual:"
    d "actual" >>= showDay
    putStrLn ""

tryAnswer :: Int -> Int -> IO ()
tryAnswer quest part = do
  Solution (s1, s2, s3) <- (days !! pred quest) "sample"
  Solution (a1, a2, a3) <- (days !! pred quest) "actual"
  let (sampleAnswer, actualAnswer) = bimap show show case part of
        1 -> (s1, a1)
        2 -> (s2, a2)
        3 -> (s3, a3)
  putStrLn $ printf "Sample answer for quest %d part %d: %s" quest part sampleAnswer
  putStrLn $ printf "Actual answer for quest %d part %d: %s" quest part actualAnswer
  putStr "Submit? "
  response <- getYNResonse
  when response do
    submitAnswer quest part actualAnswer
    when (part < 3) $ downloadNotes quest

getYNResonse :: IO Bool
getYNResonse = do
  putStr "Please answer y or n: "
  response <- getLine
  case map toUpper response of
    "Y" -> pure True
    "N" -> pure False
    _ -> getYNResonse
