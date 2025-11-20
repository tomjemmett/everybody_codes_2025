module Stories.S2.Quest02 where

import Common
import Data.List (splitAt)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as S
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

-- expectations:
s2q2_sample :: (Int, Int, Int)
s2q2_sample = (7, 2955, 2953681)

s2q2_actual :: (Int, Int, Int)
s2q2_actual = (131, 21497, 21199950)

s2q2 :: String -> IO (Int, Int, Int)
s2q2 = getInput 202 part1 part2 part3

part1 :: String -> Int
part1 = go (cycle "RGB")
  where
    go _ [] = 0
    go (x : xs) ys = 1 + go xs (drop 1 $ dropWhile (== x) ys)

part2 :: String -> Int
part2 = solve 100

part3 :: String -> Int
part3 = solve 100000

solve :: Int -> String -> Int
solve replications input = go (cycle "RGB") lq rq
  where
    i = concat $ replicate replications input
    (l, r) = splitAt (length i `div` 2) i
    lq = S.fromList l
    rq = S.fromList r
    go (y : ys) lq rq
      | S.null lq = if S.null rq then 0 else 1
      | S.null rq = 1
      | S.length lq == S.length rq && l == y = 1 + go ys lq' rq'
      | S.length lq == S.length rq = 1 + go ys lq' rq
      | otherwise = 1 + go ys lq'' rq'
      where
        (l :<| lq') = lq
        (r :<| rq') = rq
        lq'' = lq' |> r
