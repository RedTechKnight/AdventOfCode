{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day8 where

import Control.Monad.RWS.Strict
import Control.Monad.ST.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.List as List
import Data.Semigroup
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Vector (Vector (..))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector (..), STVector (..))
import qualified Data.Vector.Mutable as MVector

day8 :: IO ()
day8 = do
  input <- Text.readFile "inputs/day8"
  let (part1, Max part2) = runST $ solve input
  putStrLn . mconcat $ ["Day 8: Part 1 => ", show part1, " | Part 2 => ", show part2]

solve :: Text -> ST s (Int, Max Int)
solve input = do
  let gridWidth = Text.length . Text.takeWhile (/= '\n') $ input
      grid = Vector.unfoldrN (gridWidth * gridWidth) (fmap (bimap (\c -> (False, ord c - ord '0')) (Text.dropWhile (== '\n'))) . Text.uncons) input
  vec <- Vector.thaw grid
  evalRWST (program gridWidth gridWidth) vec Set.empty

rowCol :: Foldable t => Int -> t (Int, Int) -> Program s ()
rowCol gridWidth rc = do
  traverse_ (updateVisibility . uncurry (toIndex gridWidth)) rc
  put Set.empty

program :: Int -> Int -> Program s Int
program gridWidth gridHeight = do
  traverse_ (rowCol gridWidth) . fmap (\y -> toLeft (gridWidth, y)) $ [0 .. gridHeight - 1]
  traverse_ (rowCol gridWidth) . fmap (\y -> toRight gridWidth (-1, y)) $ [0 .. gridHeight - 1]
  traverse_ (rowCol gridWidth) . fmap (\x -> above (x, gridHeight)) $ [0 .. gridWidth - 1]
  traverse_ (rowCol gridWidth) . fmap (\x -> below gridHeight (x, -1)) $ [0 .. gridWidth - 1]
  grid <- ask
  traverse_ (calculateScore gridWidth gridHeight) [0 .. gridWidth * gridHeight - 1]
  MVector.foldl' (\acc (visible, _) -> acc + fromEnum visible) 0 grid

type Program s = RWST (STVector s (Bool, Int)) (Max Int) (Set Int) (ST s)

updateVisibility :: Int -> Program s ()
updateVisibility i = do
  vec <- ask
  (visible, height) <- MVector.read vec i
  encountered <- get
  MVector.write vec i (visible || (Set.null . Set.filter (>= height) $ encountered), height)
  modify' (Set.insert height)

calculateScore :: Int -> Int -> Int -> Program s ()
calculateScore gridWidth gridHeight i = do
  let x = mod i gridWidth
      y = div i gridWidth
  vec <- ask
  (_, height) <- MVector.read vec i
  left <- visibleTrees height . fmap (uncurry (toIndex gridWidth)) $ toLeft (x, y)
  right <- visibleTrees height . fmap (uncurry (toIndex gridWidth)) $ toRight gridWidth (x, y)
  up <- visibleTrees height . fmap (uncurry (toIndex gridWidth)) $ above (x, y)
  down <- visibleTrees height . fmap (uncurry (toIndex gridWidth)) $ below gridHeight (x, y)
  tell (Max (left * right * up * down))

visibleTrees :: Int -> [Int] -> Program s Int
visibleTrees h [] = pure 0
visibleTrees h (i : is) = do
  vec <- ask
  (_, height) <- MVector.read vec i
  fmap (+ 1) (if h > height then visibleTrees h is else pure 0)

toIndex gridWidth x y = y * gridWidth + x

above (x, y) = List.unfoldr (\y -> if (y - 1) >= 0 then Just ((x, y - 1), y - 1) else Nothing) y

below gridHeight (x, y) = List.unfoldr (\y -> if (y + 1) < gridHeight then Just ((x, y + 1), y + 1) else Nothing) y

toLeft (x, y) = List.unfoldr (\x -> if (x - 1) >= 0 then Just ((x - 1, y), x - 1) else Nothing) x

toRight gridWidth (x, y) = List.unfoldr (\x -> if (x + 1) < gridWidth then Just ((x + 1, y), x + 1) else Nothing) x
