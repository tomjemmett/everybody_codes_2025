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

type Dragon = S.HashSet Point2d

type Sheep = S.HashSet Point2d

type Hides = S.HashSet Point2d

type MaxBounds = Point2d

type Part12State = RWS (MaxBounds, Hides) () (Dragon, Sheep) Int

type Part3State = (Point2d, [Point2d])

day10 :: String -> IO (Int, Int, Int)
day10 = getInput 10 part1 part2 part3

part1 :: String -> Int
part1 = part12 4 dragonsTurn

part2 :: String -> Int
part2 = part12 20 turn

part12 :: Int -> Part12State -> String -> Int
part12 n f input = sum r
  where
    (rdr, (dragon, sheep)) = parseInputPart input
    initState = (S.singleton dragon, S.fromList sheep)
    -- detect if sample or actual
    nMoves = if head (S.toList $ fst initState) == (6, 6) then 3 else n
    (r, _, _) = runRWS (replicateM nMoves f) rdr initState

turn :: Part12State
turn = do
  d <- dragonsTurn
  s <- sheepsTurn
  pure $ d + s

dragonsTurn :: Part12State
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

sheepsTurn :: Part12State
sheepsTurn = do
  modify $ second (S.map (second succ))
  checkSheep

checkSheep :: Part12State
checkSheep = do
  (maxXY, hides) <- ask
  (dragons, sheep) <- get
  let inSheep = S.filter (\(_, y) -> y <= snd maxXY) sheep
  let unsafeSheep = S.difference inSheep hides
  let eatenSheep = S.intersection dragons unsafeSheep
  let sheep' = S.difference inSheep eatenSheep
  put (dragons, sheep')
  pure $ S.size eatenSheep

part3 :: String -> Int
part3 input = evalState (go initState) M.empty
  where
    --
    ((maxXY@(_, maxY), hides), initState) = parseInputPart input
    --
    go :: Part3State -> State (M.HashMap Part3State Int) Int
    go (dragon, []) = pure 1
    go k@(dragon, sheep) = do
      cache <- get
      case M.lookup k cache of
        Just v -> pure v
        Nothing -> do
          let possibleSheepMoves = do
                (pre, x : post) <- zip (inits sheep) (tails sheep)
                let x' = second succ x
                guard $ x' /= dragon || x' `elem` hides
                pure $ pre ++ (x' : post)
              sheep' = if null possibleSheepMoves then [sheep] else possibleSheepMoves
              nextStates = possibleDragonMoves dragon sheep'
          v <- sum <$> forM nextStates go
          modify (M.insert k v)
          pure v
    possibleDragonMoves :: Point2d -> [[Point2d]] -> [Part3State]
    possibleDragonMoves dragon sheep = do
      dragon <- moveDragon maxXY dragon
      sheep' <- filter (all ((<= maxY) . snd)) sheep
      pure (dragon, filter ((`elem` hides) <||> (/= dragon)) sheep')

parseInputPart :: String -> ((Point2d, S.HashSet Point2d), (Point2d, [Point2d]))
parseInputPart input = ((maxXY, hides), (dragon, sheep))
  where
    xs = do
      (y, line) <- zip [0 ..] (lines input)
      (x, char) <- zip [0 ..] line
      pure (char, (x, y))
    maxXY = maximum $ map snd xs
    Just dragon = snd <$> find ((== 'D') . fst) xs
    sheep = map snd $ filter ((== 'S') . fst) xs
    hides = S.fromList $ map snd $ filter ((== '#') . fst) xs
