{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day3 where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Text (Text(..))
import Data.Map (Map(..))
import qualified Data.List as List
import Data.Char
import qualified Data.Map as Map
day3 :: IO ()
day3 = do
    input <- Text.readFile "inputs/day3"
    let part1 = sum . fmap (\t -> occurring 2 . Text.chunksOf (div (Text.length t) 2) $ t) . Text.lines $ input
        part2 = sum . fmap (occurring 3) . chunks 3 . Text.lines $ input
    putStrLn . mconcat $ ["Day 3: Part 1 => ",show part1," | Part 2 => ",show part2]

occurences :: Int -> [Text] -> Map Char Int -> Map Char Int
occurences _ [] m = m 
occurences 0 (l:ls) m = occurences 1 ls (Text.foldl (\map c -> Map.insert c 1 map) m l)
occurences n (l:ls) m = occurences (n+1) ls (Text.foldl (flip $ Map.update (f n)) m l)
    where 
        f n ((>= n) -> True) = Just (n+1)
        f _ _ = Nothing


priority :: Char -> Int
priority (ord -> p) = if p < ord 'a' then p - ord 'A' + 27 else p - ord 'a' + 1 

occurring :: Int -> [Text] -> Int
occurring n = sum . fmap priority . Map.keys . Map.filter (>(n-1)) . flip (occurences 0) Map.empty

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)
