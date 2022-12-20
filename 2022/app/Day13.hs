{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day13 where

import Control.Applicative
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
import Data.Map (Map (..))
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
import Text.Parsec hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Token

day13 :: IO ()
day13 = do
  input <- Text.readFile "inputs/day13"
  let result = fromRight [] $ parse signal "" input
      divider1 = List [List [Number 2]]
      divider2 = List [List [Number 6]]
      sorted = List.sort $ divider1 : divider2 : concatMap (\(l, r) -> [l, r]) result
      part1 = sum $ zipWith (\i b -> i * fromEnum (uncurry (<) b)) [1 ..] result
      part2 = fromMaybe 0 $ liftA2 (*) ((1 +) <$> List.elemIndex divider2 sorted) ((1 +) <$> List.elemIndex divider1 sorted)
  putStrLn . mconcat $ ["Day 13: Part 1 => ", show part1, " | Part 2 => ", show part2]

packet :: Parsec Text u Packet
packet = do
  char '['
  elems <- (try (Number <$> num) <|> try packet) `sepBy` char ','
  char ']'
  pure (List elems)

num :: Parsec Text u Int
num = either (const 0) fst . Text.decimal . Text.pack <$> many1 digit

signal :: Parsec Text u [(Packet, Packet)]
signal = ((,) <$> packet <* char '\n' <*> packet) `sepBy` string "\n\n"

data Packet = Number Int | List [Packet] deriving (Show)

instance Eq Packet where
  (Number a) == (Number b) = a == b
  (List a) == (List b) = a == b
  (List a) == (Number b) = List a == List [Number b]
  (Number a) == (List b) = List [Number a] == List b

instance Ord Packet where
  compare (Number a) (Number b) = compare a b
  compare (List a) (List b) = compare a b
  compare (List a) (Number b) = compare (List a) (List [Number b])
  compare (Number a) (List b) = compare (List [Number a]) (List b)