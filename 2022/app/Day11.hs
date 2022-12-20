{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap (..))
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector (..))
import qualified Data.Vector.Mutable as MVector

day11 :: IO ()
day11 = do
  input <- Text.readFile "inputs/day11"
  let segments = Text.splitOn "\n\n" input
      monkeys = foldr' (\input monkeys -> parseMonkey input : monkeys) [] segments
      modulo = foldl' (\acc (Monkey _ test _ _ _ _) -> acc * test) 1 monkeys
      part1 = uncurry (*) . foldl' largest (0, 0) $ execState (traverse (executeRound 3 maxBound) [1 .. 20]) (IntMap.fromList . zip [0 ..] $ monkeys)
      part2 = uncurry (*) . foldl' largest (0, 0) $ execState (traverse (executeRound 1 modulo) [1 .. 10000]) (IntMap.fromList . zip [0 ..] $ monkeys)
  putStrLn . mconcat $ ["Day 11: Part 1 => ", show part1, " | Part 2 => ", show part2]

largest (a, b) (inspected -> x)
  | x > a = (x, a)
  | x > b = (a, x)
  | otherwise = (a, b)

type Program a = State (IntMap Monkey) a

executeRound d modulo _ = do
  indices <- gets IntMap.keys
  traverse_ (inspect d modulo) indices
  foldl' largest (0, 0) <$> get

inspect :: Int -> Int -> Int -> Program ()
inspect d modulo i = do
  (Monkey op test whenTrue whenFalse items inspected) <- gets (IntMap.! i)
  let newItems = fmap ((`div` d) . (`mod` modulo) . op) items
  traverse_ (\item -> throw (chooseTarget test whenTrue whenFalse item) item) newItems
  modify' (IntMap.adjust (\_ -> Monkey op test whenTrue whenFalse Seq.empty (inspected + Seq.length newItems)) i)

chooseTarget test whenTrue whenFalse item = if mod item (fromIntegral test) == 0 then whenTrue else whenFalse

throw :: Int -> Int -> Program ()
throw target item = modify' (IntMap.adjust (\(Monkey op test whenTrue whenFalse items inspected) -> Monkey op test whenTrue whenFalse (items :|> item) inspected) target)

data Monkey = Monkey
  { op :: Int -> Int,
    test :: Int,
    whenTrue :: Int,
    whenFalse :: Int,
    items :: Seq Int,
    inspected :: Int
  }

parseMonkey input = Monkey op test whenTrue whenFalse (parseItems itemsText) 0
  where
    [_, itemsText, opText, testText, whenTrueText, whenFalseText] = Text.lines input
    test = either (const 1) fst . Text.decimal . Text.dropWhile (not . isNumber) $ testText
    whenTrue = either (const 0) fst . Text.decimal . Text.dropWhile (not . isNumber) $ whenTrueText
    whenFalse = either (const 0) fst . Text.decimal . Text.dropWhile (not . isNumber) $ whenFalseText
    op
      | "old * old" `Text.isInfixOf` opText = (^ 2)
      | Text.elem '*' opText = (* (either (const 0) fst . Text.decimal . Text.dropWhile (not . isNumber) $ opText))
      | otherwise = (+ (either (const 0) fst . Text.decimal . Text.dropWhile (not . isNumber) $ opText))

parseItems :: Integral a => Text -> Seq a
parseItems (Text.decimal . Text.dropWhile (not . isNumber) -> Left _) = Seq.empty
parseItems (Text.decimal . Text.dropWhile (not . isNumber) -> Right (item, rest)) = item :<| parseItems rest