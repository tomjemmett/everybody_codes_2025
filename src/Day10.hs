module Day10 where

import Common
import Control.Monad (guard, replicateM)
import Control.Monad.RWS
import Control.Monad.State
import Data.Bifunctor (first, second)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (find, inits, tails)
import Data.Maybe (fromMaybe)
import ECSolution (Solution, getInput, makeSolution, runDay)

day10 :: String -> IO (Int, Int, Int)
day10 = getInput 10 part1 part2 part3

part1 :: String -> Int
part1 input = sum r
  where
    (rdr, initState) = parseInput input
    -- detect if sample or actual
    nMoves = if head (S.toList $ fst initState) == (6, 6) then 3 else 4
    (r, _, _) = runRWS (replicateM nMoves dragonsTurn) rdr initState

part2 :: String -> Int
part2 input = sum r
  where
    (rdr, initState) = parseInput input
    -- detect if sample or actual
    nMoves = if head (S.toList $ fst initState) == (6, 6) then 3 else 20
    (r, _, _) = runRWS (replicateM nMoves turn) rdr initState

turn :: RWS (Point2d, S.HashSet Point2d) () (S.HashSet Point2d, S.HashSet Point2d) Int
turn = do
  d <- dragonsTurn
  s <- sheepsTurn
  pure $ d + s

dragonsTurn :: RWS (Point2d, S.HashSet Point2d) () (S.HashSet Point2d, S.HashSet Point2d) Int
dragonsTurn = do
  maxXY <- fst <$> ask
  modify $ first (S.fromList . concatMap (moveDragon maxXY) . S.toList)
  checkSheep

moveDragon :: Point2d -> Point2d -> [Point2d]
moveDragon (maxX, maxY) (x, y) = filter inBounds ds
  where
    ds =
      [ (x + 1, y - 2),
        (x + 1, y + 2),
        (x - 1, y - 2),
        (x - 1, y + 2),
        (x + 2, y - 1),
        (x + 2, y + 1),
        (x - 2, y - 1),
        (x - 2, y + 1)
      ]
    inBounds (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

sheepsTurn :: RWS (Point2d, S.HashSet Point2d) () (S.HashSet Point2d, S.HashSet Point2d) Int
sheepsTurn = do
  modify $ second (S.map (second succ))
  checkSheep

checkSheep :: RWS (Point2d, S.HashSet Point2d) () (S.HashSet Point2d, S.HashSet Point2d) Int
checkSheep = do
  (maxXY, hides) <- ask
  (dragons, sheep) <- get
  let inSheep = S.filter (\(_, y) -> y <= snd maxXY) sheep
  let unsafeSheep = S.difference inSheep hides
  let eatenSheep = S.intersection dragons unsafeSheep
  let sheep' = S.difference inSheep eatenSheep
  put (dragons, sheep')
  pure $ S.size eatenSheep

parseInput :: String -> ((Point2d, S.HashSet Point2d), (S.HashSet Point2d, S.HashSet Point2d))
parseInput input = ((maxXY, hides), (dragon, sheep))
  where
    xs = do
      (y, line) <- zip [0 ..] (lines input)
      (x, char) <- zip [0 ..] line
      pure (char, (x, y))
    maxXY = maximum $ map snd xs
    Just dragon = S.singleton . snd <$> find ((== 'D') . fst) xs
    sheep = S.fromList $ map snd $ filter ((== 'S') . fst) xs
    hides = S.fromList $ map snd $ filter ((== '#') . fst) xs

--- part 3 doesn't reuse any code from part 1 or 2

part3 :: String -> Int
part3 input = evalState (go initState) M.empty
  where
    --
    ((maxXY@(_, maxY), hides), initState) = parseInputPart3 input
    --
    go :: (Point2d, [Point2d]) -> State (M.HashMap (Point2d, [Point2d]) Int) Int
    go (dragon, []) = pure 1
    go k@(dragon, sheep) = do
      cache <- get
      case M.lookup k cache of
        Just v -> pure v
        Nothing -> do
          let sheep' = if null possibleSheepMoves then [sheep] else possibleSheepMoves
              nextStates = possibleDragonMoves dragon sheep'
          v <- sum <$> forM nextStates go
          modify (M.insert k v)
          pure v
      where
        possibleSheepMoves :: [[Point2d]]
        possibleSheepMoves =
          do
            (pre, x : post) <- zip (inits sheep) (tails sheep)
            let x' = second succ x
            guard $ x' /= dragon || x' `elem` hides
            pure $ pre ++ (x' : post)

    possibleDragonMoves :: Point2d -> [[Point2d]] -> [(Point2d, [Point2d])]
    possibleDragonMoves dragon sheep = do
      dragon <- moveDragon maxXY dragon
      sheep' <- filter (all ((<= maxY) . snd)) sheep
      pure (dragon, filter ((`elem` hides) <||> (/= dragon)) sheep')

parseInputPart3 :: String -> ((Point2d, S.HashSet Point2d), (Point2d, [Point2d]))
parseInputPart3 input = ((maxXY, hides), (dragon, sheep))
  where
    xs = do
      (y, line) <- zip [0 ..] (lines input)
      (x, char) <- zip [0 ..] line
      pure (char, (x, y))
    maxXY = maximum $ map snd xs
    Just dragon = snd <$> find ((== 'D') . fst) xs
    sheep = map snd $ filter ((== 'S') . fst) xs
    hides = S.fromList $ map snd $ filter ((== '#') . fst) xs
