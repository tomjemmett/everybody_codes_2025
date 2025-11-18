module Day01 where

import Common
import Data.List (splitAt)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day01 :: String -> IO (String, String, String)
day01 = getInput 1 part1 part2 part3

part1 :: String -> String
part1 = uncurry go . f . parseInput
  where
    go :: ([String], String, [String]) -> [(Char, Int)] -> String
    go (_, name, _) [] = name
    go names (('L', n) : ms) = go (moveL names n) ms
      where
        moveL n 0 = n
        moveL n@([], _, _) _ = n
        moveL (l : ls, n, rs) i = moveL (ls, l, n : rs) (i - 1)
    go names (('R', n) : ms) = go (moveR names n) ms
      where
        moveR n 0 = n
        moveR n@(_, _, []) _ = n
        moveR (ls, n, r : rs) i = moveR (n : ls, r, rs) (i - 1)
    f :: ([String], [(Char, Int)]) -> (([String], String, [String]), [(Char, Int)])
    f (n : ns, moves) = (([], n, ns), moves)

part2 :: String -> String
part2 = uncurry go . parseInput
  where
    go :: [String] -> [(Char, Int)] -> String
    go (name : _) [] = name
    go names ((d, n) : ms) = go names' ms
      where
        n' = if d == 'L' then length names - n else n
        names' = take (length names) $ drop n' $ cycle names

part3 :: String -> String
part3 = uncurry go . parseInput
  where
    go :: [String] -> [(Char, Int)] -> String
    go (name : _) [] = name
    go names ((d, n) : ms)
      | n == 0 = go names ms
      | n == length names = go names ms
    go names ((d, n) : ms) = go names' ms
      where
        n' = (if d == 'L' then length names - n else n) `mod` length names
        (l : ls, r : rs) = splitAt n' names
        names' = (r : ls) ++ (l : rs)

parseInput :: String -> ([String], [(Char, Int)])
parseInput = parse do
  names <- commaSeparated (P.many1 P.letter) <* P.many P.newline
  moves <- commaSeparated pMove
  pure (names, moves)
  where
    pMove = do
      dir <- P.oneOf "LR"
      dist <- number
      pure (dir, dist)
