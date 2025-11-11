module Day06 where

import Common
import Data.ByteString (count)
import Data.List (inits)
import ECSolution (Solution, getInput, makeSolution, runDay)

day06 :: String -> IO (Int, Int, Int)
day06 = getInput 6 part1 part2 part3

part1 :: String -> Int
part1 = sum . map go . tail . inits
  where
    go :: String -> Int
    go xs = if last xs == 'a' then countTrue (== 'A') (init xs) else 0

part2 :: String -> Int
part2 = sum . map go . tail . inits
  where
    go xs = case last xs of
      'a' -> countTrue (== 'A') (init xs)
      'b' -> countTrue (== 'B') (init xs)
      'c' -> countTrue (== 'C') (init xs)
      _ -> 0

part3 :: String -> Int
part3 input = scan xs ys zs counts
  where
    dist = 1000
    zs = concat $ replicate 1000 input
    xs = replicate (dist + 1) '-' ++ zs
    ys = drop dist zs ++ repeat '-'
    counts = initCounts dist zs
    scan :: String -> String -> String -> (Int, Int, Int) -> Int
    scan _ _ [] _ = 0
    scan (x : xs) (y : ys) (z : zs) (a, b, c) = res + scan xs ys zs (a', b', c')
      where
        a' = a - (if x == 'A' then 1 else 0) + (if y == 'A' then 1 else 0)
        b' = b - (if x == 'B' then 1 else 0) + (if y == 'B' then 1 else 0)
        c' = c - (if x == 'C' then 1 else 0) + (if y == 'C' then 1 else 0)
        res = case z of
          'a' -> a'
          'b' -> b'
          'c' -> c'
          _ -> 0
    initCounts :: Int -> String -> (Int, Int, Int)
    initCounts dist xs = (a, b, c)
      where
        xs' = take dist xs
        a = countTrue (== 'A') xs'
        b = countTrue (== 'B') xs'
        c = countTrue (== 'C') xs'
