module Day15 where

import Algorithm.Search
import Common
import Data.IntMap.Strict qualified as IM
import Data.Set qualified as S
import Data.Vector qualified as V
import ECSolution (getInput)
import Text.Parsec qualified as P

data CoordinateCompression = CoordinateCompression
  { values :: V.Vector Int,
    index :: IM.IntMap Int
  }

makeCoordinateCompression :: S.Set Int -> CoordinateCompression
makeCoordinateCompression (S.toList -> xs) = CoordinateCompression v i
  where
    v = V.fromList xs
    i = IM.fromList $ zip xs [0 ..]

compressPoint :: (CoordinateCompression, CoordinateCompression) -> Point2d -> Point2d
compressPoint (ccX, ccY) (x, y) = (index ccX IM.! x, index ccY IM.! y)

day15 :: String -> IO (Int, Int, Int)
day15 = getInput 15 solve solve solve

parseInput :: String -> [(Char, Int)]
parseInput = parse (p `P.sepBy` P.char ',')
  where
    p = do
      d <- P.oneOf "LR"
      n <- number
      pure (d, n)

solve :: String -> Int
solve input = result
  where
    Just (result, _) = dijkstra nfn cfn goal start
    wallSegments = buildWallSegments $ parseInput input
    compression = createCompression wallSegments
    start = compressPoint compression $ fst $ head wallSegments
    end = compressPoint compression $ snd $ last wallSegments
    wall = createCompressedWall compression wallSegments
    nfn = neighbours compression wall end
    cfn = cost compression
    goal = (end ==)

cost :: (CoordinateCompression, CoordinateCompression) -> Point2d -> Point2d -> Int
cost (ccX, ccY) (x1, y1) (x2, y2) = dx + dy
  where
    dx = abs (values ccX V.! x1 - values ccX V.! x2)
    dy = abs (values ccY V.! y1 - values ccY V.! y2)

neighbours ::
  (CoordinateCompression, CoordinateCompression) ->
  S.Set Point2d ->
  Point2d ->
  Point2d ->
  [Point2d]
neighbours (ccX, ccY) wall goal = filter valid . point2dNeighbours
  where
    valid p = not (S.member p wall) || p == goal

createCompressedWall ::
  (CoordinateCompression, CoordinateCompression) ->
  [(Point2d, Point2d)] ->
  S.Set Point2d
createCompressedWall (ccX, ccY) = foldl go S.empty
  where
    go :: S.Set Point2d -> (Point2d, Point2d) -> S.Set Point2d
    go acc ((x1, y1), (x2, y2)) = S.union acc wall
      where
        [x1', x2'] = orderCoords (index ccX IM.! x1, index ccX IM.! x2)
        [y1', y2'] = orderCoords (index ccY IM.! y1, index ccY IM.! y2)
        wall = S.fromList [(x, y) | x <- [x1' .. x2'], y <- [y1' .. y2']]
    orderCoords (a, b) = if a <= b then [a, b] else [b, a]

createCompression :: [(Point2d, Point2d)] -> (CoordinateCompression, CoordinateCompression)
createCompression segments = (f fst, f snd)
  where
    f fn =
      makeCoordinateCompression $
        S.fromList
          [ fn p + i | (v1, v2) <- segments, p <- [v1, v2], i <- [-1, 0, 1]
          ]

buildWallSegments :: [(Char, Int)] -> [(Point2d, Point2d)]
buildWallSegments = go (0, 0) (0, 1)
  where
    go _ _ [] = []
    go p@(px, py) (dx, dy) ((d, n) : rest) = (p, (px', py')) : go (px', py') (dx', dy') rest
      where
        dx' = case d of
          'L' -> -dy
          'R' -> dy
        dy' = case d of
          'L' -> dx
          'R' -> -dx
        px' = px + dx' * n
        py' = py + dy' * n