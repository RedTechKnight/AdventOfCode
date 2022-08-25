module Day13 where

import System.IO
import Data.List
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- fmap lines (readFile "input_day13")
  let tracks = parseTracks input 0
      cs = getCarts tracks
  print "First crash is at: "
  print (firstCrash tracks cs)
  print "Last cart standing is at: "
  print (uncrashed tracks cs)

type Coord = (Int,Int)
type LastTurn = Int
data Dir = U | R | D | L deriving Eq
data Cart = 
  Cart 
  {
  coord :: Coord,
  dir :: Dir,
  turn :: LastTurn
  }

data Track =  
  Track 
  {
    up :: Maybe Dir,
    right :: Maybe Dir,
    down :: Maybe Dir,
    left :: Maybe Dir,
    carts :: [Cart]
  }

empTrack = Track Nothing Nothing Nothing Nothing []

instance Eq Cart where
  (==) (Cart c d t) (Cart c1 d1 t1) = c == c1 && d == d1
  (/=) cart1 cart2 = not (cart1 == cart2)

instance Show Dir where
  show U = "Up"
  show R = "Right"
  show D = "Down"
  show L = "Left"

instance Show Cart where
  show (Cart (x,y) dir lastTurn) = show (x,y) ++ " " ++ show dir 

instance Show Track where
  show (Track _ _ _ _ []) = ".."
  show (Track _ _ _ _ cs) = show cs 

parseTracks :: [String] -> Int -> [(Coord,Track)]
parseTracks [] _ = []
parseTracks (l:ls) y = concat ((parseLine ((0,y),l)):(parseTracks ls (y+1)):[])

parseLine :: (Coord,String) -> [(Coord,Track)]
parseLine (_,[]) = [] 
parseLine ((x,y),(c:cs)) =
        case c of 
        '-' -> ((x,y),hori):nextLine
        '|' -> ((x,y),vert):nextLine
        '+' -> ((x,y),all):nextLine
        '^' -> ((x,y),addCart vert (Cart (x,y) (U) 0)):nextLine
        '>' -> ((x,y),addCart hori (Cart (x,y) (R) 0)):nextLine
        'v' -> ((x,y),addCart vert (Cart (x,y) (D) 0)):nextLine
        '<' -> ((x,y),addCart hori (Cart (x,y) (L) 0)):nextLine
        '\\' -> if null cs then ((x,y),dl):nextLine else curve1
        '/' -> if null cs then ((x,y),ul):nextLine else curve2
        _ -> nextLine
      where
        nextLine = parseLine ((x+1,y),cs)
        vert = Track (Just U) Nothing (Just D) Nothing []
        hori = Track Nothing (Just R) Nothing (Just L) []
        all = Track (Just U) (Just R) (Just D) (Just L) []
        dl = Track Nothing Nothing (Just D) (Just L) []
        rd = Track Nothing (Just R) (Just D) Nothing []
        ul = Track (Just U) Nothing Nothing (Just L) []
        ur = Track (Just U) (Just R) Nothing Nothing []
        curve1 = case (head cs) of
          '-' -> ((x,y),ur):nextLine
          '+' -> ((x,y),ur):nextLine
          '>' -> ((x,y),ur):nextLine
          '<' -> ((x,y),ur):nextLine
          _ -> ((x,y),dl):nextLine
        curve2 = case (head cs) of
          '-' -> ((x,y),rd):nextLine
          '+' -> ((x,y),rd):nextLine
          '>' -> ((x,y),rd):nextLine
          '<' -> ((x,y),rd):nextLine
          _ -> ((x,y),ul):nextLine

addCart :: Track -> Cart -> Track
addCart (Track u r d l carts) cart = (Track u r d l (cart:carts))

removeCart :: Track -> Coord -> Track
removeCart (Track u r d l carts) crd = (Track u r d l filtered)
  where 
    filtered = filter (\c-> coord c /= crd) carts

getCarts :: [(Coord,Track)] -> [Cart]
getCarts [] = []
getCarts ((_,Track _ _ _ _ []):ts) = getCarts ts
getCarts ((_,Track _ _ _ _ cs):ts) = concat (cs:(getCarts ts):[])

crashes :: [(Coord,Track)] -> Bool
crashes tracks = any (\(c,t) -> (length . carts) t > 1) tracks

uncrashed :: [(Coord,Track)] -> [Cart] -> Cart
uncrashed tracks cs = if length cs == 1 then (head cs) else uncrashed removedCrashes newCarts
  where
    nextStep = simulateSystemNoCrash tracks cs
    removedCrashes = removeCrashes nextStep
    newCarts = getCarts removedCrashes

firstCrash :: [(Coord,Track)] -> [Cart] -> Coord
firstCrash tracks cs = if crashes tracks then crash else firstCrash nextStep newCarts
  where
    nextStep = stepSystem tracks cs
    newCarts = getCarts nextStep
    crash = (fst . fromMaybe undefined) (find crashed tracks)
    crashed (_,track) = (length . carts) track > 1

removeCrashes :: [(Coord,Track)] -> [(Coord,Track)]
removeCrashes tracks = if crashes tracks then removeCrashes removed else tracks
  where
    (h,tl) = break (\(_,track) -> (length . carts) track > 1) tracks
    nocrash (c,Track u r d l cs) = (c,Track u r d l []) 
    removed = concat (h:[(nocrash $ head tl)]:(drop 1 tl):[])

stepSystem :: [(Coord,Track)] -> [Cart] -> [(Coord,Track)]
stepSystem tracks [] = tracks
stepSystem tracks (c:cs) = if crashes tracks then tracks else stepSystem nextStep cs
  where
    nextStep = simulateCart tracks c

simulateSystemNoCrash :: [(Coord,Track)] -> [Cart] -> [(Coord,Track)]
simulateSystemNoCrash s [] = s
simulateSystemNoCrash tracks (c:cs) = simulateSystemNoCrash nextState finalCarts
  where
    nextState = simulateCart tracks c
    removedCrashes = removeCrashes nextState
    newCarts = getCarts removedCrashes
    finalCarts = filter (\x -> elem x newCarts) cs

simulateCart :: [(Coord,Track)] -> Cart -> [(Coord,Track)]
simulateCart tracks cart = movedCart
  where 
    current = fromMaybe undefined (lookup origCoord tracks)
    newCart = nextPos current cart 
    removed = removeFromTracks tracks origCoord 
    movedCart = insertIntoTracks removed newCart
    origCoord = coord cart

removeFromTracks :: [(Coord,Track)] -> Coord -> [(Coord,Track)]
removeFromTracks tracks crd = newTracks
  where
    (hd,tl) = break (\(c,_) -> c == crd) tracks
    oldTrack = snd $ head tl
    removedTrack = removeCart oldTrack crd
    newTracks = concat (hd:[(crd,removedTrack)]:(drop 1 tl):[])


insertIntoTracks :: [(Coord,Track)] -> Cart -> [(Coord,Track)]
insertIntoTracks tracks cart = newTracks
  where
    (hd,tl) = break (\(crd,_) -> crd == coord cart) tracks
    oldTrack = snd $ head tl
    insertedTrack = addCart oldTrack cart
    newTracks = concat (hd:[(coord cart,insertedTrack)]:(drop 1 tl):[])

nextPos :: Track -> Cart -> Cart
nextPos (Track Nothing (Just R) Nothing (Just L) _) (Cart (x,y) dir t) =
  case dir of
    R -> Cart (x+1,y) dir t
    L -> Cart (x-1,y) dir t
    _ -> undefined
nextPos (Track (Just U) Nothing (Just D) Nothing _) (Cart (x,y) dir t) =
  case dir of
    U -> Cart (x,y-1) dir t
    D -> Cart (x,y+1) dir t
    _ -> undefined
nextPos (Track (Just U) (Just R) Nothing Nothing _) (Cart (x,y) dir t) =
  case dir of 
    D -> Cart (x+1,y) R t
    L -> Cart (x,y-1) U t
    _ -> undefined
nextPos (Track (Just U) Nothing Nothing (Just L) _) (Cart (x,y) dir t) =
  case dir of
    D -> Cart (x-1,y) L t
    R -> Cart (x,y-1) U t
    _ -> undefined
nextPos (Track Nothing (Just R) (Just D) Nothing _) (Cart (x,y) dir t) = 
  case dir of
    U -> Cart (x+1,y) R t
    L -> Cart (x,y+1) D t
    _ -> undefined
nextPos (Track Nothing Nothing (Just D) (Just L) _) (Cart (x,y) dir t) =
  case dir of
    U -> Cart (x-1,y) L t
    R -> Cart (x,y+1) D t
    _ -> undefined
nextPos (Track (Just U) (Just R) (Just D) (Just L) _) (Cart (x,y) dir 0) =
  case dir of
    U -> (Cart (x-1,y) L 1)
    R -> (Cart (x,y-1) U 1)
    D -> (Cart (x+1,y) R 1)
    L -> (Cart (x,y+1) D 1)
nextPos (Track (Just U) (Just R) (Just D) (Just L) _) (Cart (x,y) dir 1) =
  case dir of
    U -> (Cart (x,y-1) U 2)
    R -> (Cart (x+1,y) R 2)
    D -> (Cart (x,y+1) D 2)
    L -> (Cart (x-1,y) L 2)
nextPos (Track (Just U) (Just R) (Just D) (Just L) _) (Cart (x,y) dir 2) =
  case dir of
    U -> (Cart (x+1,y) R 0)
    R -> (Cart (x,y+1) D 0)
    D -> (Cart (x-1,y) L 0)
    L -> (Cart (x,y-1) U 0)
nextPos _ _ = undefined