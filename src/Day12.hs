module Day12 where

import Common
import Data.Char (digitToInt)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (foldl', maximumBy)
import ECSolution (getInput)

day12 :: String -> IO (Int, Int, Int)
day12 = getInput 12 part1 part2 part3

part1 :: String -> Int
part1 input = S.size $ explode i [(0, 0)]
  where
    i = parseInput input

part2 :: String -> Int
part2 input = S.size $ explode i init
  where
    i = parseInput input
    init = [(0, 0), maximum $ M.keys i]

part3 :: String -> Int
part3 input = M.size (head i) - M.size (i !! 3)
  where
    i = iterate go $ parseInput input
    go i =
      foldl' (flip M.delete) i $
        S.toList $
          maximumBy (compare `on` S.size) $
            map (explode i . (: [])) $
              M.keys i

explode :: M.HashMap Point2d Int -> [Point2d] -> S.HashSet Point2d
explode i init = go init S.empty
  where
    go :: [Point2d] -> S.HashSet Point2d -> S.HashSet Point2d
    go [] ex = ex
    go (x : xs) ex =
      if x `S.member` ex
        then go xs ex
        else go (xs ++ exploded) ex'
      where
        v = i M.! x
        neighbors = filter (`M.member` i) $ point2dNeighbours x
        exploded = filter ((<= v) . (i M.!)) neighbors
        ex' = S.insert x ex

parseInput :: String -> M.HashMap Point2d Int
parseInput input =
  M.fromList
    [ ((i, j), digitToInt v) | (i, line) <- zip [0 ..] $ lines input, (j, v) <- zip [0 ..] line
    ]
