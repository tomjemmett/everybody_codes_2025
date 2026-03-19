{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.Y2024.Quest15 where

import Common
import Control.Lens (makeLenses, over, set, (^.))
import Data.Bifunctor (second)
import Data.Bits (bit, complement, (.&.), (.|.))
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (foldl', sort)
import Data.Sequence qualified as Seq
import ECSolution (getInput)

data Garden = Garden
  { _path :: S.HashSet Point2d,
    _start :: Point2d,
    _herbs :: M.HashMap Point2d Char
  }
  deriving (Show)

makeGarden :: S.HashSet Point2d -> Point2d -> M.HashMap Point2d Char -> Garden
makeGarden p s h = Garden {_path = p, _start = s, _herbs = h}

makeLenses ''Garden

quest15 :: String -> IO (Int, Int, Int)
quest15 = getInput 2024 15 f f f
  where
    f = search . parseGarden

parseGarden :: String -> Garden
parseGarden input = makeGarden (S.fromList $ map fst vs) start herbs
  where
    vs =
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line,
          v `notElem` "#~"
      ]
    start = fst $ head $ filter ((== 0) . fst . fst) vs
    herbs = M.fromList $ filter ((/= '.') . snd) vs

search :: Garden -> Int
search garden = go (Seq.singleton (s, startMask, 0)) (S.singleton (s, startMask))
  where
    p = garden ^. path
    h = garden ^. herbs
    s = garden ^. start

    uniqueHerbs = S.toList $ S.fromList $ M.elems h
    herbBits = M.fromList $ zip uniqueHerbs (map bit [0 :: Int ..])
    herbMasks = M.map (herbBits M.!) h
    allHerbsMask = foldl' (.|.) 0 $ M.elems herbBits

    stripHerb :: Point2d -> Integer -> Integer
    stripHerb i hs = case herbMasks M.!? i of
      Just herbMask -> hs .&. complement herbMask
      Nothing -> hs

    startMask = stripHerb s allHerbsMask

    go :: Seq.Seq (Point2d, Integer, Int) -> S.HashSet (Point2d, Integer) -> Int
    go !q !seen = case Seq.viewl q of
      Seq.EmptyL -> error "No path found"
      (i, hs, d) Seq.:< rest
        | hs == 0 && i == s -> d
        | otherwise ->
            let !neighbors = filter (`S.member` p) (point2dNeighbours i)
                (!newSeen, !newQ) = foldl' (visitStrict hs d) (seen, rest) neighbors
             in go newQ newSeen

    visitStrict ::
      Integer ->
      Int ->
      (S.HashSet (Point2d, Integer), Seq.Seq (Point2d, Integer, Int)) ->
      Point2d ->
      (S.HashSet (Point2d, Integer), Seq.Seq (Point2d, Integer, Int))
    visitStrict hs d (!seenAcc, !qAcc) !i =
      let !hs' = stripHerb i hs
          !state = (i, hs')
       in if state `S.member` seenAcc
            then (seenAcc, qAcc)
            else (S.insert state seenAcc, qAcc Seq.|> (i, hs', d + 1))

part3 :: Garden -> Int
part3 g = search lg + search rg + search mg + 8
  where
    -- in the last row, find the herbs which split the path
    -- hard coded to k in my input
    [lk, rk] = sort $ M.keys $ M.filter (== 'K') $ g ^. herbs
    lk' = second pred lk
    rk' = second succ rk
    --
    fn c = M.filterWithKey c $ g ^. herbs
    -- get the herbs in the left, middle, right
    -- (make one of the 'K's lower case, so we have to find both)
    ls = fn (\(_, k) _ -> k <= snd lk')
    ms = M.insert lk 'k' $ fn (\(_, k) _ -> k >= snd lk && k <= snd rk)
    rs = fn (\(_, k) _ -> k >= snd rk')
    -- split the path into 3 distinct areas
    g' = over path (S.delete lk' . S.delete rk') g
    -- create the left/middle/right objects
    lg = set start (second pred lk') $ set herbs ls g'
    mg = set herbs ms g'
    rg = set start (second succ rk') $ set herbs rs g'