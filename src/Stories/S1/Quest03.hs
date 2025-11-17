module Stories.S1.Quest03 where

import Common
import ECSolution (Solution, getInput, makeSolution, runDay)
import Text.Parsec qualified as P

-- expectations:
s1q3_sample :: (Integer, Integer, Integer)
s1q3_sample = (1310, 13659, 13659)

s1q3_actual :: (Integer, Integer, Integer)
s1q3_actual = (3444, 1029790, 90312279547)

s1q3 :: String -> IO (Integer, Integer, Integer)
s1q3 = getInput 103 part1 part2 part3

part1 :: String -> Integer
part1 input = sum r
  where
    i = parseInput input
    r = map (score . move 100) i
    move n (x, y) = (x', y')
      where
        m = x + y - 1
        x' = ((x + n - 1) `mod` m) + 1
        y' = m + 1 - x'
    score (x, y) = x + 100 * y

part2 :: String -> Integer
part2 = moduloReduce . map transform . parseInput
  where
    transform (x, y) = (x + y - 1, y - 1)

part3 :: String -> Integer
part3 = part2

inv :: Integer -> Integer -> Integer
inv a m =
  let (_, i, _) = egcd a m
   in i `mod` m

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
  let (g, s, t) = egcd (b `mod` a) a
   in (g, t - (b `div` a) * s, s)

moduloReduce :: [(Integer, Integer)] -> Integer
moduloReduce [(m, r)] = r
moduloReduce ((m1, r1) : (m2, r2) : rest) =
  moduloReduce ((modulo, remainder) : rest)
  where
    term1 = r1 * inv m2 m1 * m2
    term2 = r2 * inv m1 m2 * m1
    modulo = m1 * m2
    remainder = (term1 + term2) `mod` modulo

parseInput :: String -> [(Integer, Integer)]
parseInput = parse (P.many p)
  where
    p = do
      x <- toInteger <$> (P.string "x=" *> number)
      y <- toInteger <$> (P.string "y=" *> number)
      pure (x, y)
