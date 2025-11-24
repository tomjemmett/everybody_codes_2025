module Day15 where

import Algorithm.Search
import Common
import Data.Either (fromRight)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromJust)
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

data Maze = Maze
  { start :: Point2d,
    end :: Point2d,
    wall :: S.Set Point2d,
    compression :: (CoordinateCompression, CoordinateCompression)
  }

instance Show Maze where
  show Maze {..} = unlines [[f (x, y) | x <- [0 .. maxX]] | y <- reverse [0 .. maxY]]
    where
      (ccX, ccY) = compression
      maxX = V.length (values ccX) - 1
      maxY = V.length (values ccY) - 1
      f p
        | p == start = 'S'
        | p == end = 'E'
        | S.member p wall = '#'
        | otherwise = ' '

makeMaze :: Point2d -> Point2d -> S.Set Point2d -> (CoordinateCompression, CoordinateCompression) -> Maze
makeMaze s e w c = Maze {start = s, end = e, wall = w, compression = c}

data SolvedMaze = SolvedMaze
  { maze :: Maze,
    path :: [Point2d],
    cost :: Int
  }

makeSolvedMaze :: Maze -> Maybe SolvedMaze
makeSolvedMaze m = case findPath m of
  Just (cost, path) -> Just $ SolvedMaze {maze = m, path = path, cost = cost}
  Nothing -> Nothing

instance Show SolvedMaze where
  show SolvedMaze {..} = unlines [[f (x, y) | x <- [0 .. maxX]] | y <- reverse [0 .. maxY]]
    where
      (ccX, ccY) = compression maze
      maxX = V.length (values ccX) - 1
      maxY = V.length (values ccY) - 1
      pathSet = S.fromList path
      f p
        | p == start maze = 'S'
        | p == end maze = 'E'
        | S.member p pathSet = '.'
        | S.member p (wall maze) = '#'
        | otherwise = ' '

compressPoint :: (CoordinateCompression, CoordinateCompression) -> Point2d -> Point2d
compressPoint (ccX, ccY) (x, y) = (index ccX IM.! x, index ccY IM.! y)

day15 :: String -> IO (Int, Int, Int)
day15 = getInput 15 solve solve solve

parseInput :: String -> Maze
parseInput input = makeMaze start end wall compression
  where
    Right wallSegments = P.runParser (go `P.sepBy` P.char ',') ((0, 0), (0, 1)) "" input
    compression = createCompression wallSegments

    -- Get the start/end points, and the wall, in compressed coordinates
    start = compressPoint compression $ fst $ head wallSegments
    end = compressPoint compression $ snd $ last wallSegments
    wall = createCompressedWall compression wallSegments

    -- Parser to read the input
    go :: P.Parsec String (Point2d, Point2d) (Point2d, Point2d)
    go = do
      (p@(px, py), (dx, dy)) <- P.getState

      dir <- P.oneOf "LR"
      num <- read <$> P.many1 P.digit

      let (dx', dy') = case dir of
            'L' -> (-dy, dx)
            'R' -> (dy, -dx)
          px' = px + dx' * num
          py' = py + dy' * num

      P.putState ((px', py'), (dx', dy'))
      pure (p, (px', py'))

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

solve :: String -> Int
solve (parseInput -> maze) = case makeSolvedMaze maze of
  Just SolvedMaze {..} -> cost
  Nothing -> error "No path found"

findPath :: Maze -> Maybe (Int, [Point2d])
findPath Maze {..} =
  aStar nfn cfn hfn goal start
  where
    -- Functions for A* search
    nfn = neighbours compression wall end
    cfn = cost compression
    hfn = cfn end
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
