module Day12_P1 where

import Control.Monad
import System.IO

type HasPlant = Bool
type PlantProducers = [(String,HasPlant)]

main :: IO ()
main = do 
    input <- readFile "input_day12" >>= (\x -> return (lines x))
    let initial = dropWhile (\x -> x /= '#' && x /= '.') (head input)
        plants = plantProducers input
        gens = take 200 (generations (0,0,initial) plants)
        scores (i,off,str) = (i,score (off,str))
        finalState = map scores gens
        cp = constPoint finalState
    print "Score of 20th generation "
    (print . snd) (finalState !! 19)
    print "Score after 50,000,000,000 generations"
    print (snd (finalState !! (cp - 1)) + (32 * (50000000000 - (cp))))

plantProducers :: [String] -> PlantProducers
plantProducers [] = []
plantProducers str = foldr producers [] str

producers :: String -> PlantProducers -> PlantProducers
producers line arr
  |elem '=' line = case last line of
    '#' -> (take 5 line,True):arr
    _ -> arr
  |otherwise = arr

producesPlant :: String -> PlantProducers -> HasPlant
producesPlant pttn producers = 
  case lookup pttn producers of
    Just True -> True
    _ -> False

nextGeneration :: String -> PlantProducers -> String
nextGeneration ps [] = take (length ps) (repeat '.')
nextGeneration ps arr
  |len > 4 = 
    case (producesPlant (take 5 ps) arr) of 
      True -> '#':(nextGeneration next arr)
      False -> '.':(nextGeneration next arr)
  |len <= 4 && len > 0 = 
    case (producesPlant (ps ++ padding) arr) of 
      True -> '#':(nextGeneration next arr)
      False -> '.':(nextGeneration next arr)
  |otherwise = ps
    where 
      next = drop 1 ps
      len = length ps
      padding = take (5 - len) $ repeat '.'

generations :: (Int,Int,String) -> PlantProducers -> [(Int,Int,String)]
generations (n,i,str) arr = (n,index,trimmed):(generations (n+1,index,trimmed) arr)
  where 
    next = nextGeneration ("....." ++ str) arr
    trimmed = dropWhile dot (reverse (dropWhile dot (reverse next)))
    dot c = c == '.'
    index = (i - 3) + length (takeWhile dot next)

score :: (Int,String) -> Int
score (i,ps) = snd (foldl addUp (i,0) list)
  where 
    addUp (ind,total) c = if c == '#' then (ind+1,total+ind) else (ind+1,total)
    list = ps

constPoint :: [(Int,Int)] -> Int
constPoint ss = sameDiff diffs
  where 
    diffs = scanl (\d (i,v) -> (i,v,(val d) - v)) (0,0,0) ss
    sameDiff ((i,v,d):[]) = i
    sameDiff ((i1,v1,d1):(i2,v2,d2):ds) = if d1 == d2 then i1 else sameDiff ((i2,v2,d2):ds)
    val (x,y,z) = y