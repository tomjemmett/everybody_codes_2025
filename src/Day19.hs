module Day19 where

import Algorithm.Search
import Common
import Control.Monad (guard)
import Data.HashMap.Strict qualified as M
import Data.List (sort)
import Data.Vector qualified as V
import ECSolution (getInput)
import Text.Parsec qualified as P

day19 :: String -> IO (Int, Int, Int)
day19 = getInput 19 solve solve solve

solve :: String -> Int
solve (parseInput -> walls) = case search walls of
  Just c -> c
  Nothing -> error "No path found"

search :: M.HashMap Int [(Int, Int)] -> Maybe Int
search walls = fst <$> dijkstra neighbours cost goal (0, 0)
  where
    ks = sort $ M.keys walls
    maxX = maximum ks
    goal :: Point2d -> Bool
    goal (x, _) = x == maxX
    neighbours :: Point2d -> [Point2d]
    neighbours (x, y) = concatMap go $ walls M.! x'
      where
        x' : _ = dropWhile (<= x) ks
        dx = x' - x
        go (l, h) = do
          y' <- [y - dx, y - dx + 2 .. y + dx]
          guard $ l <= y' && y' <= h
          pure (x', y')
    cost :: Point2d -> Point2d -> Int
    cost (x1, y1) (x2, y2) = (y2 - y1 + x2 - x1) `div` 2

parseInput :: String -> M.HashMap Int [(Int, Int)]
parseInput = M.fromListWith (++) . parse (p `P.sepBy` P.newline)
  where
    p = do
      x <- number' <* P.char ','
      h <- number' <* P.char ','
      s <- number'
      pure (x, [(h, h + s)])
