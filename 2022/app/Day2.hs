{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day2 where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.List as List
import Text.Parsec
import Data.Either
import Data.Char
import Data.Monoid
import Data.Bifoldable
import Data.Foldable
day2 :: IO ()
day2 = do
    input <- Text.readFile "inputs/day2"
    let (Sum part1,Sum part2)= foldMap solve $ Text.lines input
    putStrLn (mconcat ["Day 2: Part 1 => ",show part1," | Part 2 => ",show part2])

points :: Char -> Int
points = (+ negate (ord 'A')) . ord . remap

remap :: Char -> Char
remap 'X' = 'A'
remap 'Y' = 'B'
remap 'Z' = 'C'
remap c = c

scoreP1 :: Integral a => a -> a -> a
scoreP1 a b|a == b = 3 + 1 + b
             |mod (a+1) 3 == b = 6 + 1 + b
             |mod (a-1) 3 == b = 0 + 1 + b

scoreP2 :: (Num a1, Integral a2, Eq a1) => a2 -> a1 -> a2
scoreP2 a 0 = mod (a-1) 3 + 1
scoreP2 a 1 = a + 1 + 3
scoreP2 a 2 = mod (a+1) 3 + 1 + 6

solve :: Text.Text -> (Sum Int, Sum Int)
solve (fmap points . Text.unpack . Text.take 3 -> [a,_,b]) = (Sum (scoreP1 a b),Sum (scoreP2 a b))
