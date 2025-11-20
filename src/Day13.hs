module Day13 where

import Common
import Data.Foldable (toList)
import Data.Sequence ((<|), (|>))
import Data.Sequence qualified as S
import ECSolution (getInput)
import Text.Parsec qualified as P

day13 :: String -> IO (Int, Int, Int)
day13 = getInput 13 part1 part2 part3
  where
    part1 = solve 2025
    part2 = solve 20252025
    part3 = solve 202520252025

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

buildSeq :: [(Int, Int)] -> [(Int, [Int])]
buildSeq values =
  take (succ $ length values) $
    dropWhile ((/= [1]) . snd) $
      cycle $
        toList (go init values)
  where
    init = S.singleton (1, [1])
    go :: S.Seq (Int, [Int]) -> [(Int, Int)] -> S.Seq (Int, [Int])
    go seq [] = seq
    go seq [(x1, x2)] = (x2 - x1 + 1, [x1 .. x2]) <| seq
    go seq ((x1, x2) : (y1, y2) : xs) =
      go seq' xs
      where
        x = (x2 - x1 + 1, [x1 .. x2])
        y = (y2 - y1 + 1, [y2, y2 - 1 .. y1])
        seq' = (y <| seq) |> x

solve :: Int -> String -> Int
solve v input = f b s
  where
    s = buildSeq $ parseInput input
    a = sum $ map fst s
    b = v `mod` a
    f i ((count, rng) : xs)
      | i >= count = f (i - count) xs
      | otherwise = rng !! i
