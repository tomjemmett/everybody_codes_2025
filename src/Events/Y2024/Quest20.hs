module Events.Y2024.Quest20 where

import Algorithm.Search
import Common
import Common qualified as P
import Data.HashMap.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.PQueue.Prio.Max qualified as PQ
import ECSolution (getInput)

type Grid = M.HashMap Point2d Char

type Input = (Grid, Point2d)

quest20 :: String -> IO (Int, Int, Int)
quest20 = getInput 2024 20 p1 p2 p3
  where
    p1 = part1 . parseInput
    p2 = part2 . parseInput
    p3 = part3 . parseInput

parseInput :: String -> (Grid, Point2d)
parseInput input = (M.fromList g, start)
  where
    g =
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line,
          v /= '#'
      ]
    start = fst . head $ filter ((== 'S') . snd) g

part3 = const 0

part1 :: Input -> Int
part1 (g, s) = foldr f 1000 $ take 100 $ map (g M.!) p ++ repeat '+'
  where
    f = \case
      '.' -> pred
      '+' -> succ
      '-' -> subtract 2
    Just (c, p) = dijkstra neighbors cost goal s
    targets = [1, 5, 10, 15, 20, 25, 29]
    neighbors (x, y) =
      [ i
        | i <- [(x + 1, y), (x, y + 1), (x, y - 1)],
          M.member i g
      ]
    cost _ p = case g M.! p of
      '+' -> 1
      '.' -> 2
      '-' -> 3
    goal (x, y) = x == 20 && y `elem` targets

part2 :: Input -> Int
part2 (g, s) = r
  where
    Just (r, _) = dijkstra neighbors cost goal (s, South, 10000, 3)
    neighbors (p, d, h, t) = mapMaybe f ds
      where
        ds = d : if d `elem` [North, South] then [East, West] else [North, South]
        f d' = case g M.!? x of
          Nothing -> Nothing
          Just '+' -> Just (x, d', h + 1, t)
          Just '-' -> Just (x, d', h - 2, t)
          Just '.' -> Just (x, d', h - 1, t)
          Just 'S' -> Just (x, d', h - 1, t)
          Just _ -> Just (x, d', h - 1, t - 1)
          where
            x = moveOneStepInDir p d'
    cost = const . const 1
    goal (p, _, h, t)
      | p /= s = False
      | h < 10000 = False
      | t > 0 = False
      | otherwise = True