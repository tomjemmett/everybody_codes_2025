module Day02 where

import Common
import Data.Maybe (isJust)
import ECSolution (Solution, getInput, makeSolution, runDay)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Complex = Complex Int Int

instance Show Complex where
  show (Complex x y) = "[" ++ show x ++ "," ++ show y ++ "]"

day02 :: String -> IO (String, Int, Int)
day02 = getInput 2 part1 part2 part3

addComplex :: Complex -> Complex -> Complex
addComplex (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

multiplyComplex :: Complex -> Complex -> Complex
multiplyComplex (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + y1 * x2)

divideComplex :: Complex -> Complex -> Complex
divideComplex (Complex x1 y1) (Complex x2 y2) = Complex x y
  where
    x = if x1 > 0 then x1 `div` x2 else -((-x1) `div` x2)
    y = if y1 > 0 then y1 `div` y2 else -((-y1) `div` y2)

part1 :: String -> String
part1 input = show r
  where
    a = parseComplex input
    r = iterate (cycleComplex a) (Complex 0 0) !! 3
    cycleComplex :: Complex -> Complex -> Complex
    cycleComplex a r0 = r3
      where
        r1 = multiplyComplex r0 r0
        r2 = divideComplex r1 b
        r3 = addComplex r2 a
        b = Complex 10 10

part2 :: String -> Int
part2 input = countTrue isJust as
  where
    a = parseComplex input
    as =
      [ fn $ addComplex a (Complex i j)
        | i <- [0, 10 .. 1001],
          j <- [0, 10 .. 1001]
      ]
    fn a' = iterate (cycleComplex a') (Just $ Complex 0 0) !! 100
    cycleComplex :: Complex -> Maybe Complex -> Maybe Complex
    cycleComplex _ Nothing = Nothing
    cycleComplex a (Just r0) =
      if inRange r3
        then Just r3
        else Nothing
      where
        r1 = multiplyComplex r0 r0
        r2 = divideComplex r1 b
        r3 = addComplex r2 a
        b = Complex 100000 100000
        inRange (Complex x y) = abs x <= 1000000 && abs y <= 1000000

part3 :: String -> Int
part3 input = countTrue isJust as
  where
    a = parseComplex input
    as =
      [ fn $ addComplex a (Complex i j)
        | i <- [0 .. 1000],
          j <- [0 .. 1000]
      ]
    fn a' = iterate (cycleComplex a') (Just $ Complex 0 0) !! 100
    cycleComplex :: Complex -> Maybe Complex -> Maybe Complex
    cycleComplex _ Nothing = Nothing
    cycleComplex a (Just r0) =
      if inRange r3
        then Just r3
        else Nothing
      where
        r1 = multiplyComplex r0 r0
        r2 = divideComplex r1 b
        r3 = addComplex r2 a
        b = Complex 100000 100000
        inRange (Complex x y) = abs x <= 1000000 && abs y <= 1000000

parseComplex :: String -> Complex
parseComplex = parse do
  name <- P.many1 P.letter
  P.string "=["
  x <- number
  P.char ','
  y <- number
  P.char ']'
  pure (Complex x y)
