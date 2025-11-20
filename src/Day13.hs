module Day13 where

import Common
import ECSolution (getInput)
import Text.Parsec qualified as P

day13 :: String -> IO (Int, Int, Int)
day13 = getInput 13 part1 part2 part3

part1 = solve 2025
part2 = solve 20252025
part3 = solve 202520252025

solve :: Int -> String -> Int
solve v input = i !! idx
  where
    i = buildSequence $ parseInput input
    n = length i
    idx = v `mod` n

buildSequence :: [(Int, Int)] -> [Int]
buildSequence values = 1 : go [] values
  where
    go :: [Int] -> [(Int, Int)] -> [Int]
    go revSeq [] = revSeq
    go revSeq [ (x1, x2) ] = [x1..x2] ++ revSeq
    go revSeq ((x1, x2):(y1, y2):xs) = [x1..x2] ++ go ([y2,y2-1..y1]++revSeq) xs

parseInput :: String -> [(Int, Int)]
parseInput = parse (p `P.sepBy` P.newline)
  where
    p = P.try p2 P.<|> p1
    p2 = do
      start <- number
      P.char '-'
      end <- number'
      pure (start, end)
    p1 = do
      start <- number'
      pure (start, start)