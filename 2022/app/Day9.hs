{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day9 where
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
import Control.Monad.State.Strict
import qualified Data.Vector.Mutable as MVector
import Data.Vector.Mutable (MVector(..))
import Control.Monad.ST.Strict
import qualified Data.Vector as Vector
day9 :: IO ()
day9 = do
    input <- Text.readFile "inputs/day9"
    let moves = concatMap parse (Text.lines input)
        (part1,_) = first Set.size $ foldl' simulate (Set.empty,Seq.replicate 2 (0,0)) moves
        (part2,_) = first Set.size $ foldl' simulate (Set.empty,Seq.replicate 10 (0,0)) moves
    putStrLn . mconcat $ ["Day 9: Part 1 => ",show part1," | Part 2 => ",show part2]

parse :: (Num a, Num b) => Text -> [(a, b)]
parse l@(Text.take 1 -> "U") = replicate (either (const 0) fst . Text.decimal . Text.drop 2 $ l) (0,1)
parse l@(Text.take 1 -> "D") = replicate (either (const 0) fst . Text.decimal . Text.drop 2 $ l) (0,-1)
parse l@(Text.take 1 -> "L") = replicate (either (const 0) fst . Text.decimal . Text.drop 2 $ l) (-1,0)
parse l@(Text.take 1 -> "R") = replicate (either (const 0) fst . Text.decimal . Text.drop 2 $ l) (1,0)
parse _ = []

nextMove :: (Integral a2, Integral a1) => (a1, a2) -> (a1, a2)
nextMove (dX,dY)|dX == 0 && abs dY == 2 = (0,div dY 2)
              |dY == 0 && abs dX == 2 = (div dX 2,0)
              |abs dX == 1 && abs dY == 2 = (dX,div dY 2)
              |abs dY == 1 && abs dX == 2 = (div dX 2,dY)
              |abs dX == 2 && abs dY == 2 = (div dX 2,div dY 2)
              |otherwise = (0,0)

simulate :: (Integral b, Integral a) => (Set (a, b), Seq (a, b)) -> (a, b) -> (Set (a, b), Seq (a, b))
simulate (visited,(hX,hY) :<| tails) (dX,dY) = (Set.insert tail visited,rope)
    where
        (hX',hY') = (hX+dX,hY+dY)
        rope = Seq.scanl (\(hX,hY) (tX,tY) -> let (x,y) = nextMove (hX-tX,hY-tY) in (tX+x,tY+y)) (hX',hY') tails
        (_ :|> tail) = rope