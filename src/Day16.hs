module Day16 where

import Common
import ECSolution (getInput)

day16 :: String -> IO (Int, Int, Int)
day16 = getInput 16 part1 part2 part3

part1 :: String -> Int
part1 = sum . take 90 . buildWall . commaSeparatedInts

part2 :: String -> Int
part2 = product . getSpell . commaSeparatedInts

part3 :: String -> Int
part3 input = binarySearch 1 (t `div` 2)
  where
    xs = getSpell $ commaSeparatedInts input
    t = 202520252025000
    binarySearch :: Int -> Int -> Int
    binarySearch low high
      | low + 1 >= high = low
    binarySearch low high =
      if sum (map (mid `div`) xs) < t
        then binarySearch mid high
        else binarySearch low mid
      where
        mid = (low + high) `div` 2

getSpell :: [Int] -> [Int]
getSpell = go 1
  where
    go :: Int -> [Int] -> [Int]
    go cur xs
      | all (== 0) xs = []
      | any (< 0) xs' = go (succ cur) xs
      | otherwise = cur : go (succ cur) xs'
      where
        xs' = zipWith (-) xs (cycle (replicate (cur - 1) 0 ++ [1]))

buildWall :: [Int] -> [Int]
buildWall = go (repeat 0)
  where
    go :: [Int] -> [Int] -> [Int]
    go acc [] = acc
    go acc (x : xs) = go acc' xs
      where
        acc' = zipWith (+) (cycle (replicate (x - 1) 0 ++ [1])) acc
