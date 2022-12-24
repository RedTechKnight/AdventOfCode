{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Day14 where

import Control.Monad.RWS.Strict
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Text.Parsec hiding ((<|>),State(..))
import Text.Parsec.Char
import Text.Parsec.Token

day14 :: IO ()
day14 = do
  input <- Text.readFile "inputs/day14"
  let lines = fromRight [] $ traverse (runParser path () "") (Text.lines input)
      walls = foldl' addLines Set.empty lines
      lowest = (\[_,fromY,_,toY] -> max fromY toY) $ maximumBy (comparing (\[_,fromY,_,toY] -> max fromY toY)) walls
      (part1,sand,_) = runRWS (sandAtRest ((>= lowest) . snd) Nothing) walls Set.empty 
      part2 = (+1) . fst . evalRWS (sandAtRest (== (500,0)) Nothing) (Set.insert [minBound,lowest+2,maxBound,lowest+2] walls) $ sand
  putStrLn . mconcat $ ["Day 14: Part 1 => ",show part1," | Part 2 => ",show part2]

type Program a = RWS (Set [Int]) () (Set (Int,Int)) a

sandAtRest :: ((Int,Int) -> Bool) -> Maybe (Int,Int) -> Program Int
sandAtRest stopCondition Nothing = sandAtRest stopCondition (Just (500,0))
sandAtRest stopCondition (Just position) = do
    sand <- get
    walls <- ask
    let nextPosition = fromMaybe position . find (\x -> isNothing (find (noWall x) walls) && noSand x) $ [second (+1) position,bimap (+ negate 1) (+ 1) position,bimap (+1) (+1) position]
        noSand position = Set.notMember position sand 
        noWall (x,y) [fromX,fromY,toX,toY] = and [fromX <= x,toX >= x,fromY <= y,toY >= y]
    case nextPosition of
        (stopCondition -> True) -> gets Set.size
        ((== position) -> True) -> modify' (Set.insert nextPosition) *> sandAtRest stopCondition Nothing
        _ -> sandAtRest stopCondition (Just nextPosition)

addLines set ((aX,aY):(bX,bY):lines) = addLines (Set.insert [min aX bX ,min aY bY,max aX bX,max aY bY] set) $ (bX,bY):lines
addLines set _ = set

coord :: Parsec Text u (Int,Int)
coord = (,) <$> num <* char ',' <*> num 

path :: Parsec Text u [(Int,Int)]
path = coord `sepBy` string " -> "

num :: Parsec Text u Int
num = either (const 0) fst . Text.decimal . Text.pack <$> many1 digit
