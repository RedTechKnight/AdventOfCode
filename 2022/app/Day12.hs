{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Day12 where

import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap (..))
import qualified Data.List as List
import Data.Map (Map (..),(!))
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
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

day12 :: IO ()
day12 = do
  input <- Text.readFile "inputs/day12"
  let (map,start,end) = parseHeightmap input
      Just (Node part1 _ _) = Map.lookup end . fst $ execState (shortestPaths False start) (map,Seq.empty)
      part2 = minimumBy (comparing dist) . Map.filter (\(Node _ _ h) -> h == 0) . fst $ execState (shortestPaths True end) (Map.adjust (\(Node _ v h) -> Node 0 v h) end . fmap (\(Node _ v h) -> Node maxBound v h) $ map,Seq.empty)

  putStrLn . mconcat $ ["Day 12: Part 1 => ", show part1," | Part 2 => ",show (dist part2)]

type Program a = State (Map (Int, Int) Node,Seq (Int,Int)) a

neighbourNodes :: Bool -> (Int, Int) -> Program (Seq (Int, Int))
neighbourNodes backwards (x, y) = do
  nodes <- gets fst
  let (Just (Node _ _ height)) = Map.lookup (x, y) nodes
      canClimb maxHeight (Just (Node _ False height)) = if backwards then (maxHeight - 1) <= height else height <= (maxHeight + 1)
      canClimb _ _ = False
  pure . Seq.fromList . filter (canClimb height . (`Map.lookup` nodes)) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

updateDistance :: Int -> (Int, Int) -> Program ()
updateDistance dist dest = do
  modify' (first $ Map.adjust (\(Node prevDist v h) -> Node (min (dist + 1) prevDist) v h) dest)

shortestPaths :: Bool -> (Int,Int) -> Program ()
shortestPaths backwards source = do
  (Node dist visited height) <- gets ((! source) . fst)
  unless visited $ do
    modify' (first $ Map.adjust (\(Node dist _ h) -> Node dist True h) source)
    neighbours <- neighbourNodes backwards source
    traverse_ (updateDistance dist) neighbours
    modify' (second (<> neighbours))
  gets snd >>= \case
    Seq.Empty -> pure ()
    (next :<| nodes) -> modify' (second $ Seq.drop 1) *> shortestPaths backwards next

data Node = Node {dist :: Int,visited :: Bool,height :: Int} deriving Show
parseHeightmap :: Text -> (Map (Int,Int) Node,(Int,Int),(Int,Int))
parseHeightmap = fst . Text.foldl' fillHeightmap ((Map.empty,(0,0),(0,0)),(0,0)) where
  fillHeightmap ((map,_,end),(x,y)) 'S' = ((Map.insert (x,y) (Node 0 False 0) map,(x,y),end),(x+1,y))
  fillHeightmap ((map,start,_),(x,y)) 'E' = ((Map.insert (x,y) (Node maxBound False 25) map,start,(x,y)),(x+1,y))
  fillHeightmap ((map,start,end),(x,y)) '\n' = ((map,start,end),(0,y+1))
  fillHeightmap ((map,start,end),(x,y)) c = ((Map.insert (x,y) (Node maxBound False (ord c - ord 'a')) map,start,end),(x+1,y))