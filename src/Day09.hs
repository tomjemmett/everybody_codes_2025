module Day09 where

import Common
import Data.Foldable (asum)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P

day09 :: String -> IO (Int, Int, Int)
day09 = getInput 9 part1 part2 part3

part1 :: String -> Int
part1 input = product r
  where
    (i : xs) = reverse $ map snd $ parseInput input
    r = map (matches i) xs
    matches :: String -> String -> Int
    matches xs = countTrue id . zipWith (==) xs

part2 :: String -> Int
part2 (map snd . parseInput -> input) = sum $ mapMaybe p2 input
  where
    p2 :: String -> Maybe Int
    p2 child =
      asum
        [go (0, 0) child a b | a <- input, child /= a, b <- input, child /= b]
      where
        go (t1, t2) [] _ _ = Just $ t1 * t2
        go (t1, t2) (i : is) (x : xs) (y : ys)
          | i /= x && i /= y = Nothing
          | i /= x = go (t1, t2 + 1) is xs ys
          | i /= y = go (t1 + 1, t2) is xs ys
          | otherwise = go (t1 + 1, t2 + 1) is xs ys

part3 :: String -> Int
part3 input = sum $ maximumBy (compare `on` length) $ findComponents $ buildGraph $ map p3 i
  where
    i = parseInput input
    p3 :: (Int, String) -> Maybe (Int, (Int, Int))
    p3 (childId, child) =
      asum
        [go child aId a bId b | (aId, a) <- i, child /= a, (bId, b) <- i, child /= b, aId < bId]
      where
        go :: String -> Int -> String -> Int -> String -> Maybe (Int, (Int, Int))
        go [] xId _ yId _ = Just (childId, (xId, yId))
        go (i : is) xId (x : xs) yId (y : ys)
          | i /= x && i /= y = Nothing
          | otherwise = go is xId xs yId ys

buildGraph :: [Maybe (Int, (Int, Int))] -> M.HashMap Int [Int]
buildGraph = foldl f M.empty
  where
    f g = \case
      Nothing -> g
      (Just (i, (x, y))) -> M.unionWith (++) g $ M.fromList [(i, [x, y]), (x, [i]), (y, [i])]

findComponents :: M.HashMap Int [Int] -> [[Int]]
findComponents graph = go S.empty (M.keys graph) []
  where
    go :: S.HashSet Int -> [Int] -> [[Int]] -> [[Int]]
    go _ [] components = components
    go visited (node : nodes) components
      | S.member node visited = go visited nodes components
      | otherwise = go (S.union visited componentNodes) nodes (S.toList componentNodes : components)
      where
        componentNodes = dfs S.empty [node]
        dfs :: S.HashSet Int -> [Int] -> S.HashSet Int
        dfs seen [] = seen
        dfs seen (n : ns)
          | S.member n seen = dfs seen ns
          | otherwise = dfs (S.insert n seen) (neighbors ++ ns)
          where
            neighbors = M.lookupDefault [] n graph ++ reverseEdges n
        reverseEdges :: Int -> [Int]
        reverseEdges target = [from | (from, tos) <- M.toList graph, target `elem` tos]

parseInput :: String -> [(Int, String)]
parseInput = parse do
  P.try line `P.sepBy` P.newline
  where
    line = do
      n <- number <* P.char ':'
      xs <- P.many $ P.oneOf "ATCG"
      pure (n, xs)
