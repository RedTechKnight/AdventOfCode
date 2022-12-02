{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day1 where

import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.List  as List
import Data.Monoid
import qualified Data.Text  as Text
import qualified Data.Text.IO  as Text
import qualified Data.Text.Read  as Text
import Data.Text (Text (..))

day1 :: IO ()
day1 = do
  input <- Text.readFile "inputs/day1"
  let [a, b, c] = foldl top3 [0, 0, 0] . Text.splitOn "\n\n" $ input
  putStrLn (mconcat ["Day 1: Part 1 => ", show a, " | Part 2 => ", show (a + b + c)])

totalCalories :: [Text] -> Int
totalCalories = getSum . foldMap (Sum . either (const 0) fst . Text.decimal)

top3 :: [Int] -> Text -> [Int]
top3 [a, b, c] (totalCalories . Text.lines -> x)
  | x > a = [x, a, b]
  | x > b = [a, x, b]
  | x > c = [a, b, x]
  | otherwise = [a, b, c]