{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day6 where
import Data.Char
import Data.Either
import Data.Foldable
import Data.Bifunctor
import Data.Map (Map (..))
import  qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq (..))
import qualified Data.Sequence  as Seq
import Data.Text (Text (..))
import qualified  Data.Text as Text
import qualified  Data.Text.IO as Text
import qualified Data.Text.Read  as Text
import Data.Set (Set(..))
import qualified Data.Set as Set
day6 :: IO ()
day6 = do
    input <- Text.readFile "inputs/day6"
    putStrLn . mconcat $ ["Day 6: Part 1 => ",show (findMarker 4 0 input)," | Part 2 => ",show (findMarker 14 0 input)]

unique :: Int -> Text -> Bool
unique len = (==len) . Set.size . Text.foldr Set.insert Set.empty

findMarker :: Int -> Int -> Text -> Int
findMarker len n t = if unique len (Text.take len t) then n + len else findMarker len (n+1) (Text.drop 1 t)