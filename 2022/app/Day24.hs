{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day24 where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

day24 :: IO ()
day24 = do
  input <- Text.readFile "inputs/day24"
  let map = parseMap input
      (firstTrip, Valley tiles _ _) = runState findFastestTime (Valley map (Map.singleton (1, 0) 0) True)
      (returnTrip, Valley tiles' _ _) = runState findFastestTime (Valley tiles (fmap (const 0) . uncurry Map.singleton $ Map.findMax tiles) False)
      finalTrip = evalState findFastestTime (Valley tiles' (Map.singleton (1, 0) 0) True)
  putStrLn . mconcat $ ["Day 24: Part 1 => ", show firstTrip, " | Part 2 => ", show $ firstTrip + returnTrip + finalTrip]

data Valley = Valley
  { tiles :: Map (Int, Int) [(Int, Int)],
    paths :: Map (Int, Int) Int,
    forward :: Bool
  }
  deriving (Show)

type Program a = State Valley a

findFastestTime :: Program Int
findFastestTime = do
  (Valley tiles paths forward) <- get
  let newTiles = Map.foldlWithKey' updateTile Map.empty tiles
      updateTile map tile = foldl' (move tile) (Map.alter (Just . fromMaybe []) tile map)
      move (x, y) map (xVel, yVel) = case Map.lookup (x + xVel, y + yVel) tiles of
        Nothing -> replace map (x, y) (xVel, yVel)
        Just _ -> Map.alter (\bs -> ((xVel, yVel) :) <$> Just (fromMaybe [] bs)) (x + xVel, y + yVel) map
      replace map (_, y) (1, 0) = (\x -> Map.alter (\bs -> ((1, 0) :) <$> Just (fromMaybe [] bs)) (x, y) map) . minimum . fmap fst . filter ((== y) . snd) . Map.keys $ tiles
      replace map (_, y) (-1, 0) = (\x -> Map.alter (\bs -> ((-1, 0) :) <$> Just (fromMaybe [] bs)) (x, y) map) . maximum . fmap fst . filter ((== y) . snd) . Map.keys $ tiles
      replace map (x, _) (0, 1) = (\y -> Map.alter (\bs -> ((0, 1) :) <$> Just (fromMaybe [] bs)) (x, y) map) . minimum . fmap snd . filter ((== x) . fst) . Map.keys $ tiles
      replace map (x, _) (0, -1) = (\y -> Map.alter (\bs -> ((0, -1) :) <$> Just (fromMaybe [] bs)) (x, y) map) . maximum . fmap snd . filter ((== x) . fst) . Map.keys $ tiles
      available current = filter (\coord -> Map.lookup coord newTiles == Just []) [current, first (+ 1) current, first (+ negate 1) current, second (+ 1) current, second (+ negate 1) current]
      newPaths = Map.foldlWithKey' newPosition Map.empty paths
      newPosition positions pos time = foldl' (flip (Map.alter (fmap (min (time + 1)) . Just . fromMaybe (time + 1)))) positions (available pos)
      check False = Map.findMin
      check True = Map.findMax
  modify' (const (Valley newTiles newPaths forward))
  if fst (check forward newTiles) == fst (check forward newPaths)
    then pure . snd . check forward $ newPaths
    else findFastestTime

parseMap :: Text -> Map (Int, Int) [(Int, Int)]
parseMap = fst . Text.foldl' fillMap (Map.empty, (0, 0))
  where
    fillMap (map, (x, y)) '.' = (Map.insert (x, y) [] map, (x + 1, y))
    fillMap (map, (x, y)) '>' = (Map.insert (x, y) [(1, 0)] map, (x + 1, y))
    fillMap (map, (x, y)) 'v' = (Map.insert (x, y) [(0, 1)] map, (x + 1, y))
    fillMap (map, (x, y)) '<' = (Map.insert (x, y) [(-1, 0)] map, (x + 1, y))
    fillMap (map, (x, y)) '^' = (Map.insert (x, y) [(0, -1)] map, (x + 1, y))
    fillMap (map, (x, y)) '\n' = (map, (0, y + 1))
    fillMap (map, (x, y)) _ = (map, (x + 1, y))
