{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day15 where

import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.Sequence (Seq (..), ViewL (..))
import qualified Data.Sequence as Seq
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

day15 :: IO ()
day15 = do
  input <- Text.readFile "inputs/day15"
  let ranges = fmap parseCoord (Text.lines input)
      (part1 :<| _) = rangeLength <$> joinAll (Seq.filter (not . null) . fmap (sensorRangeAt 2000000) . Seq.fromList $ ranges)
      (Just part2) = fmap (uncurry (+) . first ((* 4000000) . (+ 1) . (!! 1) . minimum)) . find ((> 1) . Seq.length . fst) . fmap (\i -> (,i) . joinAll . joinAll . Seq.filter (not . null) . fmap (sensorRangeAt i) $ Seq.fromList ranges) $ [0 .. 4000000]
  putStrLn . mconcat $ ["Day 15: Part 1 => ", show part1, " | Part 2 => ", show part2]

rangeLength [a, b] = b - a

joinAll (range :<| Seq.Empty) = Seq.singleton range
joinAll (range :<| ranges) = if Seq.length joined == 1 + Seq.length ranges then joined else joinAll joined
  where
    tryJoin range Seq.Empty = Seq.singleton range
    tryJoin range (r :<| rs) = case joinSensorRange r range of
      [r'] -> r' :<| rs
      _ -> r :<| tryJoin range rs
    (_ :|> joined) = Seq.iterateN (3 + Seq.length ranges) (tryJoin range) ranges

joinSensorRange :: [Int] -> [Int] -> [[Int]]
joinSensorRange a b
  | b1 <= (a2 + 1) = [[a1, max a2 b2]]
  | otherwise = []
  where
    [a1, a2] = min a b
    [b1, b2] = max a b

parseCoord :: Text -> [Int]
parseCoord = fmap (either (const 0) fst . Text.signed Text.decimal) . filter (not . Text.null) . fmap (Text.filter (\c -> isNumber c || c == '-')) . Text.splitOn "="

sensorRangeAt y [sensorX, sensorY, beaconX, beaconY]
  | maxX < minX = []
  | otherwise = [minX, maxX]
  where
    maxRange = abs (sensorX - beaconX) + abs (sensorY - beaconY)
    dist = abs (sensorY - y)
    minX = sensorX - maxRange + dist
    maxX = sensorX + maxRange - dist
 