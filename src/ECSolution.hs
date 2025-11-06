{-# LANGUAGE InstanceSigs #-}

module ECSolution (getInput, runDay, showDay, makeSolution, Solution) where

import Text.Printf (printf)

newtype Solution = Solution (PartResult, PartResult, PartResult)

makeSolution :: (ToPartResult p1, ToPartResult p2, ToPartResult p3) => (p1, p2, p3) -> Solution
makeSolution (p1, p2, p3) = Solution (makePartResult p1, makePartResult p2, makePartResult p3)

data PartResult = StringResult String | IntResult Int

instance Show PartResult where
  show :: PartResult -> String
  show = \case
    StringResult x -> x
    IntResult x -> show x

class ToPartResult a where
  toPartResult :: a -> PartResult

instance ToPartResult String where
  toPartResult :: String -> PartResult
  toPartResult = StringResult

instance ToPartResult Int where
  toPartResult :: Int -> PartResult
  toPartResult = IntResult

makePartResult :: (ToPartResult a) => a -> PartResult
makePartResult = toPartResult

createPath :: String -> Int -> Int -> String
createPath = printf "inputs/%s/%02d/%d.txt"

getInput ::
  Int -> -- Day
  (String -> i1) -> -- Part 1 parsing function
  (String -> i2) -> -- Part 2 parsing function
  (String -> i3) -> -- Part 3 parsing function
  String -> -- actual/sample
  IO (i1, i2, i3)
getInput d f1 f2 f3 n = do
  let path = createPath n d
  i1 <- f1 <$> readFile (path 1)
  i2 <- f2 <$> readFile (path 2)
  i3 <- f3 <$> readFile (path 3)
  pure (i1, i2, i3)

runDay ::
  (i1 -> a1) -> -- Part 1 function
  (i2 -> a2) -> -- Part 2 function
  (i3 -> a3) -> -- Part 3 function
  (i1, i2, i3) -> -- parsed inputs
  (a1, a2, a3)
runDay f1 f2 f3 (i1, i2, i3) = (f1 i1, f2 i2, f3 i3)

showDay :: Solution -> IO ()
showDay (Solution (p1, p2, p3)) = do
  print p1
  print p2
  print p3
