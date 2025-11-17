{-# LANGUAGE TemplateHaskell #-}

module Stories.S1.Quest02 where

import Common
import Control.Lens
import Control.Monad.Writer
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximumBy)
import ECSolution (Solution, getInput, makeSolution, runDay)
import Text.Parsec qualified as P

data Trees = Trees
  { _leftTree :: Tree,
    _rightTree :: Tree
  }
  deriving (Show)

data Tree = EmptyTree | Node Int (Int, Char) Tree Tree deriving (Show)

data Input = Add Int (Int, Char) | Swap Int (Int, Char) deriving (Show)

type MyParser = P.Parsec String (M.HashMap Int ((Int, Char), (Int, Char)))

type P3Parser = P.Parsec String Trees

makeLenses ''Trees

-- expectations:
s1q2_sample :: (String, String, String)
s1q2_sample = ("CFGNLK", "MGFLNK", "DJCGL")

s1q2_actual :: (String, String, String)
s1q2_actual = ("QUACK!HLFFJZHF", "QUACK!VPNGTBYBWMFTNL", "QUACK!PMVJPJGJNXHBSNZYXTMZJGZLNJVG")

s1q2 :: String -> IO (String, String, String)
s1q2 = getInput 102 part1 part2 part3

part1 :: String -> String
part1 = solve treeSwapP2

part2 :: String -> String
part2 = solve treeSwapP2

part3 :: String -> String
part3 = solve treeSwapP3

solve :: (Int -> Tree -> Tree -> Tree) -> String -> String
solve swapFn input = concatMap longestLevel [leftLevels, rightLevels]
  where
    Right trees = parseInput swapFn input
    leftLevels = traverseTree (trees ^. leftTree)
    rightLevels = traverseTree (trees ^. rightTree)
    longestLevel =
      snd
        . maximumBy (compare `on` fst)
        . zipWith (\i v -> ((length v, i), v)) [0 ..]
        . reverse

traverseTree :: Tree -> [String]
traverseTree t = go [t]
  where
    go [] = []
    go xs = level : go next
      where
        (next, level) = runWriter $ traverseTreeLevel xs

traverseTreeLevel :: [Tree] -> Writer String [Tree]
traverseTreeLevel [] = pure []
traverseTreeLevel (EmptyTree : xs) = traverseTreeLevel xs
traverseTreeLevel ((Node _ (_, n) l r) : xs) = do
  tell [n]
  xs' <- traverseTreeLevel xs
  pure $ l : r : xs'

parseInput :: (Int -> Tree -> Tree -> Tree) -> String -> Either P.ParseError Trees
parseInput swapFn = P.runParser (pLine `P.sepEndBy` P.newline >> P.getState) initTrees ""
  where
    initTrees = Trees {_leftTree = EmptyTree, _rightTree = EmptyTree}
    pLine :: P3Parser ()
    pLine = P.try pAdd P.<|> pSwap
    pAdd :: P3Parser ()
    pAdd = do
      i <- P.string "ADD id=" *> number
      -- left tree
      l <- pValueName <* P.space
      P.modifyState $ over leftTree (treeAdd (i, l))
      -- right tree
      r <- pValueName
      P.modifyState $ over rightTree (treeAdd (i, r))
      pure ()
    pSwap :: P3Parser ()
    pSwap = do
      i <- P.string "SWAP " *> number'
      t <- P.getState
      --
      let lTree = t ^. leftTree
          rTree = t ^. rightTree
          lTree' = swapFn i lTree rTree
          rTree' = swapFn i rTree lTree
      --
      P.putState $ Trees {_leftTree = lTree', _rightTree = rTree'}
      --
      pure ()
    pValueName :: P.Parsec String Trees (Int, Char)
    pValueName = do
      P.choice $ P.string <$> ["left", "right"]
      P.string "=["
      v <- number
      P.char ','
      n <- P.anyChar
      P.char ']'
      pure (v, n)
    number :: P3Parser Int
    number = read <$> P.many1 P.digit <* P.spaces
    number' :: P3Parser Int
    number' = read <$> P.many1 P.digit

treeAdd :: (Int, (Int, Char)) -> Tree -> Tree
treeAdd (nodeId, vn) EmptyTree = Node nodeId vn EmptyTree EmptyTree
treeAdd a@(_, x@(i, _)) (Node nodeId y@(j, _) l r)
  | i == j = Node nodeId x l r
  | i < j = Node nodeId y (treeAdd a l) r
  | i > j = Node nodeId y l (treeAdd a r)

treeSwapP2 :: Int -> Tree -> Tree -> Tree
treeSwapP2 swapId t1 t2 = go t1
  where
    Node _ x _ _ = findSwap [t2]
    ---
    findSwap [] = error "Shouldn't reach this state"
    findSwap (EmptyTree : xs) = findSwap xs
    findSwap (n@(Node nId v left right) : xs)
      | nId == swapId = n
      | otherwise = findSwap (left : right : xs)
    go :: Tree -> Tree
    go EmptyTree = EmptyTree
    go n@(Node nid v left right)
      | nid == swapId = Node nid x left right
      | otherwise = Node nid v (go left) (go right)

treeSwapP3 :: Int -> Tree -> Tree -> Tree
treeSwapP3 swapId t1 t2
  | null swapFromThisSide = t1
  | length swapFromOtherSide == 1 = replaceSwap t1 $ head swapFromOtherSide
  | otherwise = doubleSwap t1 swapFromThisSide
  where
    swapFromOtherSide = findSwaps [t2]
    swapFromThisSide = findSwaps [t1]
    ---
    findSwaps [] = []
    findSwaps (EmptyTree : xs) = findSwaps xs
    findSwaps (n@(Node nId v left right) : xs)
      | nId == swapId = n : findSwaps (left : right : xs)
      | otherwise = findSwaps (left : right : xs)
    ---
    replaceSwap EmptyTree _ = EmptyTree
    replaceSwap n@(Node nId v left right) swap@(Node _ x _ _)
      | nId == swapId && v /= x = swap
      | otherwise = Node nId v (replaceSwap left swap) (replaceSwap right swap)
    ---
    doubleSwap :: Tree -> [Tree] -> Tree
    doubleSwap EmptyTree _ = EmptyTree
    doubleSwap n@(Node nId v left right) swaps@[swap1@(Node _ x _ _), swap2@(Node _ y _ _)]
      | nId == swapId && v == x = swap2
      | nId == swapId && v == y = swap1
      | otherwise = Node nId v (doubleSwap left swaps) (doubleSwap right swaps)
