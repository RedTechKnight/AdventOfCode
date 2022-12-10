{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day10 where
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
day10 :: IO ()
day10 = do
    input <- Text.readFile "inputs/day10"
    let instructions = fmap parse . Text.lines $ input
        (_,_,result,pixels) = execState (traverse execute instructions) (1,1,0,[])
    putStrLn . mconcat $ ["Day 10: Part 1 => ",show result," | Part 2 => "]
    traverse_ Text.putStrLn . Text.chunksOf 40 . Text.reverse . Text.pack $ pixels

data Op = Add Int | Noop deriving (Eq,Ord,Show)

parse :: Text -> Op
parse (Text.take 4 -> "noop") = Noop
parse t@(Text.take 4 -> "addx") = Add . either (const 0) fst . Text.signed Text.decimal $ Text.drop 5 t

intervals :: [Int]
intervals = [20,60,100,140,180,220]

execute :: Op -> State (Int,Int,Int,[Char]) ()
execute Noop = do
    (cycle,register,signal,pixels) <- get
    let newSignal = signal +  register * cycle * fromEnum (cycle `elem`  intervals)
        newPixels =p (cycle-1) register : pixels  
    modify' (\(c,r,_,_) -> (c+1,r,newSignal,newPixels))
execute (Add x) = do
    (cycle,register,signal,pixels) <- get
    let newSignal = signal + (sum . fmap (\cycle -> register * cycle * fromEnum (cycle `elem` intervals)) $ [cycle,cycle+1])
        newPixels = p cycle register : p (cycle-1) register :  pixels
    modify' (\(c,r,_,_) -> (c+2,r+x,newSignal,newPixels))

p :: Integral a => a -> a -> Char
p cycle register = if mod cycle 40 `elem` [register-1,register,register+1] then '#' else '.'
