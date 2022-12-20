{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day18 where

import Control.Applicative
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap (..))
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Semigroup
import Data.Sequence (Seq (..), ViewL (..))
import qualified Data.Sequence as Seq
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector (..), STVector (..))
import qualified Data.Vector.Mutable as MVector
import Debug.Trace (trace)
import Text.Parsec hiding (State (..), (<|>))
import Text.Parsec.Char
import Text.Parsec.Token ()

day18 :: IO ()
day18 = do
  input <- Text.readFile "inputs/day18"
  let cubes = Set.fromList $ fmap parseCube (Text.lines input)
      part1 = foldl' (\acc x -> acc + freeSides cubes x) 0 cubes
      [(minX, maxX), (minY, maxY), (minZ, maxZ)] = bounds cubes
      (_, _, Sum part2) = runRWS (calculateSurfaceArea (ZipList [minX - 1, minY - 1, minZ - 1])) (cubes, [(minX, maxX), (minY, maxY), (minZ, maxZ)]) Set.empty
  putStrLn . mconcat $ ["Day 18: Part 1 => ", show part1, " | Part 2 => ", show part2]

type Program = RWS (Set (ZipList Int), [(Int, Int)]) (Sum Int) (Set (ZipList Int))

calculateSurfaceArea :: ZipList Int -> Program ()
calculateSurfaceArea current =
  get >>= \visited ->
    if Set.member current visited
      then pure ()
      else do
        (cubes, bounds) <- ask
        visited <- get
        let [(minX, maxX), (minY, maxY), (minZ, maxZ)] = bounds
            fs = [ZipList [(+ negate 1), id, id], ZipList [(+ 1), id, id], ZipList [id, (+ negate 1), id], ZipList [id, (+ 1), id], ZipList [id, id, (+ negate 1)], ZipList [id, id, (+ 1)]]
            adjacent = fmap (<*> current) fs
            occupied = length . filter (`Set.member` cubes) $ adjacent
            next = filter (\(ZipList [x, y, z]) -> x >= (minX - 1) && x <= (maxX + 1) && y >= (minY - 1) && y <= (maxY + 1) && z >= (minZ - 1) && z <= (maxZ + 1)) . filter (`Set.notMember` cubes) $ adjacent
        tell (Sum occupied)
        modify' (Set.insert current)
        traverse_ calculateSurfaceArea next

parseCube :: Text -> ZipList Int
parseCube = ZipList . either (const []) (fmap fst) . traverse Text.decimal . Text.splitOn ","

freeSides :: Set (ZipList Int) -> ZipList Int -> Int
freeSides cubes cube = length free
  where
    fs = [ZipList [(+ negate 1), id, id], ZipList [(+ 1), id, id], ZipList [id, (+ negate 1), id], ZipList [id, (+ 1), id], ZipList [id, id, (+ negate 1)], ZipList [id, id, (+ 1)]]
    free = filter (`Set.notMember` cubes) $ fmap (<*> cube) fs

bounds (toList -> cubes) = [(minX, maxX), (minY, maxY), (minZ, maxZ)]
  where
    (minX, maxX) = foldl' minMax (maxBound, 0) $ fmap (head . getZipList) cubes
    (minY, maxY) = foldl' minMax (maxBound, 0) $ fmap ((!! 1) . getZipList) cubes
    (minZ, maxZ) = foldl' minMax (maxBound, 0) $ fmap ((!! 2) . getZipList) cubes
    minMax (mn, mx) x = (min x mn, max x mx)