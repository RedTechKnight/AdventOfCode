{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Day20 where

import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad
import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.Either
import Data.Foldable
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap (..))
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
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
import Text.Parsec hiding ((<|>),State(..))
import Text.Parsec.Char
import Text.Parsec.Token ()

import Control.Monad.RWS.Strict
import Data.Semigroup
import Debug.Trace (trace)
import Data.Foldable
day20 :: IO ()
day20 = do
    input <- Text.readFile "inputs/day20"
    let numbers = Seq.fromList . fmap parseNum . Text.lines $ input
        indices = Seq.mapWithIndex const numbers
        indices2 = fold $ Seq.replicate 10 indices
        part1 = wrappedIndex . fmap snd . snd $ execState rearrange (indices,Seq.mapWithIndex (,) numbers)
        part2 = wrappedIndex . fmap snd . snd $ execState rearrange (indices2, Seq.mapWithIndex (\i num -> (i,num* 811589153)) numbers)

    putStrLn . mconcat $ ["Day 20: Part 1 => ",
        show . sum . mapMaybe part1 $ [1000,2000,3000],
        " | Part 2 => ",show . sum . mapMaybe part2 $ [1000,2000,3000]]
    pure ()

wrappedIndex seq ind = case Seq.elemIndexL 0 seq of
    Nothing -> Nothing
    Just i -> Just $ Seq.index seq (mod (i+ind) (Seq.length seq))

rearrange :: State (Seq Int,Seq (Int,Int)) () 
rearrange = do
    (indices,numbers) <- get
    case indices of
        Seq.Empty -> pure ()
        (index :<| indices) -> do
            let (left,(ind,num) :<| right) = Seq.splitAt prevIndex numbers
                prevIndex = fromJust $ Seq.findIndexL (\(i,_) -> i == index) numbers
                newIndex = mod (num + prevIndex) (Seq.length numbers - 1)
                newNumbers = if newIndex > prevIndex then left Seq.>< Seq.insertAt (newIndex - prevIndex) (ind,num) right 
                    else Seq.insertAt newIndex (ind,num) left Seq.>< right
            modify' (const (indices,newNumbers))
            rearrange

parseNum = either (const 0) fst . Text.signed Text.decimal