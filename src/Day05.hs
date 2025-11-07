module Day05 where

import Common
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import ECSolution (Solution, getInput, makeSolution, runDay)
import Text.Parsec qualified as P

type Spine = (Maybe Int, Int, Maybe Int)

day05 :: String -> IO (Int, Int, Int)
day05 = getInput 5 part1 part2 part3

part1 :: String -> Int
part1 = score . snd . head . parseInput

part2 :: String -> Int
part2 input = maximum i - minimum i
  where
    i = map (score . snd) $ parseInput input

part3 :: String -> Int
part3 input = sum $ zipWith (*) (map fst i) [1 ..]
  where
    i = sortBy compareSpines $ parseInput input

compareSpines :: (Int, [Spine]) -> (Int, [Spine]) -> Ordering
compareSpines (_, xs) (_, ys)
  | xsScore < ysScore = GT
  | xsScore > ysScore = LT
  | otherwise = go xs ys
  where
    xsScore = score xs
    ysScore = score ys
    go [] _ = GT
    go _ [] = LT
    go (x : xs) (y : ys)
      | lx < ly = GT
      | lx > ly = LT
      | otherwise = go xs ys
      where
        lx = level x
        ly = level y

level :: Spine -> Int
level (Nothing, x, Nothing) = x
level (Just l, x, Nothing) = read (show l ++ show x)
level (Nothing, x, Just r) = read (show x ++ show r)
level (Just l, x, Just r) = read (show l ++ show x ++ show r)

score :: [Spine] -> Int
score = read . concatMap (show . (\(_, x, _) -> x))

parseInput :: String -> [(Int, [Spine])]
parseInput = map parseLine . lines
  where
    parseLine = parse do
      id <- number <* P.char ':'
      xs <- P.sepBy number (P.try $ P.char ',')
      pure (id, go [] xs)
    go :: [Spine] -> [Int] -> [Spine]
    go spines [] = spines
    go spines (n : ns) = go (updateSpines spines) ns
      where
        updateSpines [] = [(Nothing, n, Nothing)]
        updateSpines (s@(l, x, r) : xs)
          | x < n && isNothing l = (Just n, x, r) : xs
          | x > n && isNothing r = (l, x, Just n) : xs
          | otherwise = s : updateSpines xs