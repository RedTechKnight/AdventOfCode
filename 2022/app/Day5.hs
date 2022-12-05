{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day5 where

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

day5 :: IO ()
day5 = do
  input <- Text.readFile "inputs/day5"
  let (stackLines : moveLines : _) = Text.splitOn "\n\n" input
      stacks = foldl (fmap (. Text.drop 1) (parseStack 1)) Map.empty (Text.lines stackLines)
      (part1, part2) = bimap f f $ foldl simulate (stacks, stacks) (Text.lines moveLines)
      f = toList . foldMap (Seq.take 1)
  putStrLn . mconcat $ ["Day 5: Part 1 => ", part1, " | Part 2 => ", part2]

putOnStack :: a -> Maybe (Seq a) -> Maybe (Seq a)
putOnStack x Nothing = Just (Seq.singleton x)
putOnStack x (Just xs) = Just (xs :|> x)

parseStack :: (Num k, Ord k) => k -> Map k (Seq Char) -> Text -> Map k (Seq Char)
parseStack i m (Text.uncons -> Just (' ', cs)) = parseStack (i + 1) m (Text.drop 3 cs)
parseStack i m (Text.uncons -> Just (c, cs)) = parseStack (i + 1) (Map.alter (putOnStack c) i m) (Text.drop 3 cs)
parseStack i m _ = m

parseMove :: Integral b => Text -> [Either String b]
parseMove "" = []
parseMove line = let l = Text.dropWhile (not . isNumber) line in (fst <$> Text.decimal (Text.takeWhile isNumber l)) : parseMove (Text.dropWhile isNumber l)

simulate :: (Map Int (Seq Char), Map Int (Seq Char)) -> Text -> (Map Int (Seq Char), Map Int (Seq Char))
simulate (stacksP1, stacksP2) (rights . parseMove -> [count, from, to]) = (stacksP1', stacksP2')
  where
    toMoveP1 = Seq.reverse $ Seq.take count (stacksP1 Map.! from)
    toMoveP2 = Seq.take count (stacksP2 Map.! from)
    stacksP1' = Map.adjust (toMoveP1 <>) to (Map.adjust (Seq.drop count) from stacksP1)
    stacksP2' = Map.adjust (toMoveP2 <>) to (Map.adjust (Seq.drop count) from stacksP2)