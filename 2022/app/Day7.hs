{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Day7 where
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
day7 :: IO ()
day7 = do
    input <- Text.readFile "inputs/day7"
    let commands = Text.lines input
        totalSpace = 70000000
        neededSpace = 30000000
        tree = snd . execState (traverse execute commands) $ ("",Map.empty)
        freeSpace = totalSpace - (tree Map.! "/")
        toDelete = minimum $ Map.filter ((>neededSpace) . (+ freeSpace)) tree
        under100000 = sum . Map.filter (<100000) $ tree
    putStrLn . mconcat $ ["Day 7: Part 1 => ",show under100000," | Part 2 => ",show toDelete]



execute :: Text ->  State (Text,Map Text Integer) ()
execute (Text.take 4 -> "$ ls") = pure ()
execute (Text.take 6 -> "$ cd /") = modify' (\(_,tree) -> ("/",Map.alter cd "/" tree))
execute (Text.take 7 -> "$ cd ..") = do
    pwd <- gets fst
    let (root,pwd') = (Text.take 1 pwd,Text.drop 1 pwd)
        newDir = root <> ( Text.dropEnd 1  . Text.dropWhileEnd (/= '/') $ pwd')
    modify' (\(dir,tree) -> (newDir,tree))

execute t@(Text.take 4 -> "$ cd") = do
    pwd <- gets fst
    let name = Text.drop 5 t
        newPwd = if pwd == "/" then pwd <> name else pwd <> "/" <> name
    modify' (\(dir,tree) -> (newPwd,Map.alter cd newPwd tree))

execute t@((>0) . Text.length . Text.filter isNumber -> True) = do
    (pwd,tree) <- get
    let (Right (size,_)) = Text.decimal (Text.takeWhile isNumber t)
        parents = filter (`Text.isPrefixOf` pwd) (Map.keys tree) 
    traverse_ (\parent -> modify' (second $ Map.alter (count size) parent)) parents
execute _ = pure ()

count :: Num a => a -> Maybe a -> Maybe a
count x Nothing = Just x
count x (Just a) = Just $ a + x

cd :: Num a => Maybe a -> Maybe a
cd Nothing = Just 0
cd j = j  