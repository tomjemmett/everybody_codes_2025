module Day16 where

import Common
import ECSolution (getInput)

day16 :: String -> IO (Int, Int, Int)
day16 = getInput 16 part1 part2 part3

part1 :: String -> Int
part1 = flip countBlocks 90 . commaSeparatedInts

part2 :: String -> Int
part2 = product . getSpell . commaSeparatedInts

part3 :: String -> Int
part3 = binarySearch . countBlocks . getSpell . commaSeparatedInts
  where
    t = 202520252025000
    binarySearch :: (Int -> Int) -> Int
    binarySearch fn = go 1 (t `div` 2)
      where
        go low high
          | low + 1 >= high = low
          | r == t = mid
          | r < t = go mid high
          | otherwise = go low mid
          where
            mid = (low + high) `div` 2
            r = fn mid

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

countBlocks :: [Int] -> Int -> Int
countBlocks xs n = sum $ map (n `div`) xs
