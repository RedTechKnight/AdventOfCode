{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Day23 where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

day23 :: IO ()
day23 = do
  input <- Text.readFile "inputs/day23"
  let map = parseMap input
      (part2, (_, _, part1)) = runState (executeRounds 1) (cycle moves, map, Set.empty)
      height = (+ 1) . abs . uncurry (-) . Set.foldl' minmax (maxBound, 0) . Set.map snd $ part1
      width = (+ 1) . abs . uncurry (-) . Set.foldl' minmax (maxBound, 0) . Set.map fst $ part1
  putStrLn . mconcat $ ["Day 23: Part 1 => ", show (width * height - Set.size part1), " | Part 2 => ", show part2]

minmax (mn, mx) x = (min mn x, max mx x)

adjacents :: [(Int, Int) -> (Int, Int)]
adjacents =
  [ bimap (+ negate 1) (+ negate 1),
    second (+ negate 1),
    bimap (+ 1) (+ negate 1),
    bimap (+ negate 1) (+ 1),
    second (+ 1),
    bimap (+ 1) (+ 1),
    first (+ negate 1),
    first (+ 1)
  ]

moves :: [[(Int, Int) -> (Int, Int)]]
moves =
  [ [bimap (+ negate 1) (+ negate 1), second (+ negate 1), bimap (+ 1) (+ negate 1)], -- north
    [bimap (+ negate 1) (+ 1), second (+ 1), bimap (+ 1) (+ 1)], -- south
    [bimap (+ negate 1) (+ 1), first (+ negate 1), bimap (+ negate 1) (+ negate 1)], -- west
    [bimap (+ 1) (+ negate 1), first (+ 1), bimap (+ 1) (+ 1)] -- east
  ]

executeRounds :: Int -> State ([[(Int, Int) -> (Int, Int)]], Set (Int, Int), Set (Int, Int)) Int
executeRounds n = do
  (moves, elves, _) <- get
  let proposedMoves = Set.foldl' possibleMove Map.empty elves
      possibleMove :: Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Map (Int, Int) [(Int, Int)]
      possibleMove map elve@(shouldMove -> True) = maybe map (\[_, newPos, _] -> Map.alter (add elve) newPos map) . find (Set.null . Set.intersection elves . Set.fromList) $ fmap (fmap ($ elve)) . take 4 $ moves
      possibleMove map _ = map
      add pos Nothing = Just [pos]
      add pos (Just ps) = Just $ pos : ps
      newElves = Map.foldlWithKey' move elves proposedMoves
      move elves newPos [elve] = Set.insert newPos . Set.delete elve $ elves
      move elves newPos _ = elves
      shouldMove elve = not . Set.null . Set.intersection elves . Set.fromList . fmap ($ elve) $ adjacents
  modify' (\(_, _, round10) -> (drop 1 moves, newElves, if n == 10 then newElves else round10))
  if Map.null . Map.filter (\l -> length l == 1) $ proposedMoves
    then pure n
    else executeRounds (n + 1)

parseMap :: Text -> Set (Int, Int)
parseMap = snd . Text.foldl' fillMap ((0, 0), Set.empty)
  where
    fillMap ((col, row), map) c
      | c == '#' = ((col + 1, row), Set.insert (col, row) map)
      | c == '\n' = ((0, row + 1), map)
      | otherwise = ((col + 1, row), map)