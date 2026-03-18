module Stories.S3.Quest02 where

import Common
import Data.Char (isUpper)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximum, maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P

data Scale = Scale
  { scaleId :: Int,
    values :: [Int]
  }
  deriving (Show)

quest02 :: String -> IO (Int, Int, Int)
quest02 = getInput 3 2 part1 part2 part3

part1 :: String -> Int
part1 = const 0

part2 :: String -> Int
part2 = const 0

part3 :: String -> Int
part3 = const 0