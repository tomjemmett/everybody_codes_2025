module Day04 where

import Common
import ECSolution (getInput)
import Text.Parsec ((<|>))
import Text.Parsec qualified as P

day04 :: String -> IO (Int, Int, Int)
day04 = getInput 4 part1 part2 part3

parseInput :: String -> [Int]
parseInput = map read . lines

getRatio :: [Double] -> Double
getRatio = product . (zipWith (/) <*> tail)

solve :: String -> Double
solve = getRatio . map fromIntegral . parseInput

part1 :: String -> Int
part1 = floor . (2025 *) . solve

part2 :: String -> Int
part2 = ceiling . (10000000000000 /) . solve

part3 :: String -> Int
part3 input = floor $ (100 *) $ product $ zipWith ratio i (tail i)
  where
    i = parseInput input
    parseInput :: String -> [(Double, Double)]
    parseInput = parse (P.many (P.try pPair <|> pNumber))
      where
        pPair = do
          l <- fromIntegral <$> number
          P.char '|'
          r <- fromIntegral <$> number
          pure (l, r)
        pNumber = fmap ((\x -> (x, x)) . fromIntegral) number
    ratio :: (Double, Double) -> (Double, Double) -> Double
    ratio (_, x) (y, _) = x / y
