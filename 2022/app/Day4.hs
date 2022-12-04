{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day4 where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Text (Text(..))
import Data.Either
import Data.Char
import Data.Monoid
day4 :: IO ()
day4 = do
    input <- Text.readFile "inputs/day4"
    let (Sum part1,Sum part2) = foldMap (count . values) . Text.lines $ input
    putStrLn . mconcat $ ["Day 4: Part 1 => ",show part1," | Part 2 => ",show part2]

values :: Text -> [Int]
values =  either (const []) (fmap fst) . traverse Text.decimal . foldMap (Text.splitOn ",") . Text.splitOn "-"

overlappingTotal :: Ord a => [a] -> Bool
overlappingTotal [a1,b1,a2,b2] = or [a1 >= a2 && b1 <= b2,a2 >= a1 && b2 <= b1] 

overlappingPartial :: Ord a => [a] -> Bool
overlappingPartial [a1,b1,a2,b2] = or [a1 >= a2 && a1 <= b2,b1 >= a2 && b1 <= a2,a2 >= a1 && a2 <= b1,b2 >= a1 && b2 <= b1]

count :: Ord a => [a] -> (Sum Int, Sum Int)
count [] = (Sum 0,Sum 0)
count ranges = (Sum . fromEnum . overlappingTotal $ ranges,Sum . fromEnum . overlappingPartial $ ranges)