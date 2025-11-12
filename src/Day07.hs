module Day07 where

import Common
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Graph.Inductive (new)
import Data.HashMap.Internal.Debug (valid)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import ECSolution (Solution, getInput, makeSolution, runDay)
import Text.Parsec qualified as P

type Rules = M.HashMap Char [Char]

day07 :: String -> IO (String, Int, Int)
day07 = getInput 7 part1 part2 part3

parts :: ([String] -> [String] -> Rules -> a) -> String -> a
parts fn input = fn validNames names rules
  where
    (names, rules) = parseInput input
    validNames = runRules names rules

part1 :: String -> String
part1 = parts (\x _ _ -> head x)

part2 :: String -> Int
part2 = parts fn
  where
    fn v n _ = sum $ map fst $ filter ((`elem` v) . snd) $ zip [1 ..] n

part3 :: String -> Int
part3 = parts fn
  where
    fn v _ r =
      S.size $
        S.filter ((>= 7) . length) $
          foldl (makeNames r) S.empty $
            map reverse v

runRules :: [String] -> M.HashMap Char [Char] -> [String]
runRules names rules = filter f names
  where
    f :: String -> Bool
    f [] = error "Shouldn't reach this state"
    f [_] = True
    f (x1 : x2 : xs) = case M.lookup x1 rules of
      Just ys -> (x2 `elem` ys) && f (x2 : xs)
      Nothing -> False

makeNames ::
  M.HashMap Char [Char] ->
  S.HashSet String ->
  String ->
  S.HashSet String
makeNames rules currentNames name = execState (go name) currentNames
  where
    go :: String -> State (S.HashSet String) ()
    go name | length name > 11 = pure ()
    go name@(x : _) = do
      seen <- get
      if S.member name seen
        then pure ()
        else do
          put (S.insert name seen)
          for_ (M.lookup x rules) $
            mapM_ (go . (: name))

parseInput :: String -> ([String], M.HashMap Char [Char])
parseInput = parse do
  names <- P.many P.letter `P.sepBy` P.char ',' <* P.newline <* P.newline
  rules <- parseRule `P.sepEndBy` P.newline
  pure (names, M.fromList rules)
  where
    parseRule = do
      x <- P.letter <* P.string " > "
      ys <- P.letter `P.sepBy` P.char ','
      pure (x, ys)
