{-# LANGUAGE TemplateHaskell #-}

module Day06 where

import Common
import Control.Lens (makeLenses, use, (+=), (^.))
import Control.Monad.State.Strict
import Data.List (inits)
import ECSolution (getInput)

data Counts = Counts
  { _countA :: !Int,
    _countB :: !Int,
    _countC :: !Int,
    _accum :: !Int
  }
  deriving (Show)

makeLenses ''Counts

makeCounts :: Int -> Int -> Int -> Counts
makeCounts a b c = Counts {_countA = a, _countB = b, _countC = c, _accum = 0}

day06 :: String -> IO (Int, Int, Int)
day06 = getInput 6 part1 part2 part3

part1 :: String -> Int
part1 = sum . map go . tail . inits
  where
    go :: String -> Int
    go xs = if last xs == 'a' then countTrue (== 'A') (init xs) else 0

part2 :: String -> Int
part2 = sum . map go . tail . inits
  where
    go xs = case last xs of
      'a' -> countTrue (== 'A') (init xs)
      'b' -> countTrue (== 'B') (init xs)
      'c' -> countTrue (== 'C') (init xs)
      _ -> 0

part3 :: String -> Int
part3 input = solve ^. accum
  where
    dist = 1000
    repeats = 1000
    ---
    xs = concat $ replicate repeats input
    ls = replicate (dist + 1) '-' ++ xs
    rs = drop dist xs ++ repeat '-'
    ---
    initCounts :: Counts
    initCounts = flip execState (makeCounts 0 0 0) $
      forM_ (take dist xs) $ \case
        'A' -> countA += 1
        'B' -> countB += 1
        'C' -> countC += 1
        _ -> return ()
    ---
    solve :: Counts
    solve = flip execState initCounts $
      forM_ (zip3 ls xs rs) $ \(l, x, r) -> do
        -- character d distance to the left, which is being removed
        updateCount (-1) l
        -- character d distance to the right, which is being added
        updateCount 1 r
        -- add to the result based on the current character
        count <- getCount x
        accum += count
    updateCount :: Int -> Char -> State Counts ()
    updateCount n = \case
      'A' -> countA += n
      'B' -> countB += n
      'C' -> countC += n
      _ -> return ()
    getCount :: Char -> State Counts Int
    getCount = \case
      'a' -> use countA
      'b' -> use countB
      'c' -> use countC
      _ -> pure 0
