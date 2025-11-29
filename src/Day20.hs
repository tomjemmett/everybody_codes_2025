module Day20 where

import Algorithm.Search
import Common
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximumBy)
import ECSolution (getInput)

day20 :: String -> IO (Int, Int, Int)
day20 = getInput 20 part1 part2 part3

part1 :: String -> Int
part1 input = length $ concatMap (canReach i) $ M.keys $ M.filter (== 'T') i
  where
    i = parseInput input
    canReach :: M.HashMap Point2d Char -> Point2d -> [Point2d]
    canReach m (x, y) = filter isValid neighbors
      where
        neighbors :: [Point2d]
        neighbors
          | even x && even y = [(x + 1, y)]
          | even x || even y = [(x + 1, y), (x, y + 1)]
          | otherwise = [(x + 1, y)]
        isValid p = case M.lookup p m of
          Just 'T' -> True
          _ -> False

part2 :: String -> Int
part2 = solve [id]

part3 :: String -> Int
part3 = solve [id, rotateBy120, rotateBy120 . rotateBy120]
  where
    rotateBy120 :: M.HashMap Point2d Char -> M.HashMap Point2d Char
    rotateBy120 m = M.fromList $ goRow (0, 0) bottom
      where
        ks = M.keys m
        bottom = maximumBy (compare `on` snd) ks
        maxX = maximum $ map fst ks
        goRow (nx, ny) (sx, sy)
          | sx == maxX = [((nx, ny), m M.! (sx, sy))]
          | otherwise = go nx (sx, sy) ++ goRow (nx + 1, ny + 1) (sx + 1, sy - 1)
          where
            go :: Int -> Point2d -> [(Point2d, Char)]
            go i (x, 0) = [((i, ny), m M.! (x, 0))]
            go i (x, y) =
              ((i, ny), m M.! (x, y))
                : ((i + 1, ny), m M.! (x, y - 1))
                : go (i + 2) (x - 1, y - 1)

solve :: [M.HashMap Point2d Char -> M.HashMap (Int, Int) Char] -> String -> Int
solve transformations input = case dijkstra neighbors cost goal (start, 0) of
  Just r -> fst r
  Nothing -> error "No path found"
  where
    i = parseInput input
    start = head $ M.keys $ M.filter (== 'S') i
    is = transformations <*> [i]
    cost _ _ = 1
    goal (p, _) = i M.! p == 'E'
    neighbors ((x, y), n) =
      map (,n')
        . filter ((/= '#') . flip (M.lookupDefault '#') i')
        $ xss : [(x + 1, y), (x - 1, y), (x, y)]
      where
        xss
          | even x && even y = (x, y - 1)
          | even x = (x, y + 1)
          | even y = (x, y + 1)
          | otherwise = (x, y - 1)
        n' = (n + 1) `mod` length transformations
        i' = is !! n'

parseInput :: String -> M.HashMap Point2d Char
parseInput input =
  M.fromList $
    concat
      [ [ ((x, y), v)
          | (x, v) <- zip [0 ..] ln,
            v /= '.'
        ]
        | (y, ln) <- zip [0 ..] $ lines input
      ]