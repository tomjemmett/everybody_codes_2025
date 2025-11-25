{-# LANGUAGE TemplateHaskell #-}

module Stories.S1.Quest02 where

import Common
import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad.Writer
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximumBy)
import ECSolution (getInput)
import Text.Parsec qualified as P

data Trees = Trees
  { _leftTree :: Tree,
    _rightTree :: Tree
  }
  deriving (Show)

data Tree = EmptyTree | Node Int (Int, Char) Tree Tree deriving (Show)

data Input = Add Int (Int, Char) | Swap Int (Int, Char) deriving (Show)

type Parser = P.Parsec String Trees

type SwapFn = Int -> Trees -> Trees

makeLenses ''Trees

s1q2 :: String -> IO (String, String, String)
s1q2 = getInput 102 part1 part2 part3

part1 :: String -> String
part1 = solve (const id)

part2 :: String -> String
part2 = solve treeSwapP2
  where
    treeSwapP2 :: Int -> Trees -> Trees
    treeSwapP2 swapId trees = Trees {_leftTree = swapFn t1 t2, _rightTree = swapFn t2 t1}
      where
        t1 = trees ^. leftTree
        t2 = trees ^. rightTree
        swapFn :: Tree -> Tree -> Tree
        swapFn t1 t2 = go t1
          where
            Node _ x _ _ = findSwap [t2]
            ---
            findSwap :: [Tree] -> Tree
            findSwap [] = error "Shouldn't reach this state"
            findSwap (EmptyTree : xs) = findSwap xs
            findSwap (n@(Node nId v left right) : xs)
              | nId == swapId = n
              | otherwise = findSwap (left : right : xs)
            ---
            go :: Tree -> Tree
            go EmptyTree = EmptyTree
            go n@(Node nid v left right)
              | nid == swapId = Node nid x left right
              | otherwise = Node nid v (go left) (go right)

part3 :: String -> String
part3 = solve treeSwapP3
  where
    treeSwapP3 :: Int -> Trees -> Trees
    treeSwapP3 swapId trees = Trees {_leftTree = swapFn t1 t2, _rightTree = swapFn t2 t1}
      where
        t1 = trees ^. leftTree
        t2 = trees ^. rightTree
        swapFn :: Tree -> Tree -> Tree
        swapFn t1 t2
          | null swapFromThisSide = t1
          | length swapFromOtherSide == 1 = replaceSwap t1 $ head swapFromOtherSide
          | otherwise = doubleSwap t1 swapFromThisSide
          where
            swapFromOtherSide = findSwaps [t2]
            swapFromThisSide = findSwaps [t1]
            ---
            findSwaps :: [Tree] -> [Tree]
            findSwaps [] = []
            findSwaps (EmptyTree : xs) = findSwaps xs
            findSwaps (n@(Node nId v left right) : xs)
              | nId == swapId = n : findSwaps (left : right : xs)
              | otherwise = findSwaps (left : right : xs)
            ---
            replaceSwap :: Tree -> Tree -> Tree
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

solve :: SwapFn -> String -> String
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

parseInput :: SwapFn -> String -> Either P.ParseError Trees
parseInput swapFn = P.runParser (pLine `P.sepEndBy` P.newline >> P.getState) initTrees ""
  where
    initTrees = Trees {_leftTree = EmptyTree, _rightTree = EmptyTree}
    pLine :: Parser ()
    pLine = P.try pAdd P.<|> pSwap
    pAdd :: Parser ()
    pAdd = do
      i <- P.string "ADD id=" *> number'
      forM_ [leftTree, rightTree] $ \t -> do
        P.modifyState . over t . treeAdd . (i,) =<< pValueName
    pSwap :: Parser ()
    pSwap = do
      i <- P.string "SWAP " *> number'
      P.modifyState $ swapFn i
    pValueName :: Parser (Int, Char)
    pValueName = do
      P.space *> P.choice (P.string <$> ["left", "right"])
      P.between (P.string "=[") (P.char ']') $ (,) <$> number' <* P.char ',' <*> P.anyChar
    number' :: Parser Int
    number' = read <$> P.many1 P.digit

treeAdd :: (Int, (Int, Char)) -> Tree -> Tree
treeAdd (nodeId, vn) EmptyTree = Node nodeId vn EmptyTree EmptyTree
treeAdd a@(_, x@(i, _)) (Node nodeId y@(j, _) l r)
  | i == j = Node nodeId x l r
  | i < j = Node nodeId y (treeAdd a l) r
  | i > j = Node nodeId y l (treeAdd a r)
