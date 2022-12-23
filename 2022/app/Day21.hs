{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import Data.Char
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Vector as Vector

day21 :: IO ()
day21 = do
    input <- Text.readFile "inputs/day21"
    let env = Map.fromList . fmap parseExpr $ Text.lines input
        part1 = eval env (Var "root")
        part2 = eval (Map.adjust (\(Expr _ a b) -> Equal a b) "root" . Map.delete "humn" $ env) (Var "root")
    putStrLn . mconcat $ ["Day 21: Part 1 => ", show part1," | Part 2 => ",show part2]

eval :: Map Text Expr -> Expr -> Expr
eval env (Equal a b) = equate (eval env a) (eval env b)
eval env (Result i) = Result i
eval env (Var x) = case Map.lookup x env of
    Just (Expr op a b) -> eval env (Expr op (eval env a) (eval env b))
    Just a -> eval env a
    Nothing -> Var x
eval env (Expr Add (Result a) (Result b)) = Result (a + b) 
eval env (Expr Add a b) = Expr Add a b
eval env (Expr Sub (Result a) (Result b)) = Result (a - b)
eval env (Expr Sub a b) = Expr Sub a b
eval env (Expr Mul (Result a) (Result b)) = Result (a * b)
eval env (Expr Mul a b) = Expr Mul a b
eval env (Expr Div (Result a) (Result b)) = Result (div a b)
eval env (Expr Div a b) = Expr Div a b


equate :: Expr -> Expr -> Expr
equate  (Result a) (Result b) = Result b
equate  (Expr Add (Result a) c) (Result b) = equate c (Result $ b - a)
equate (Expr Add a (Result c)) (Result b) = equate  a (Result $ b - c)
equate (Expr Sub (Result a) c) (Result b) = equate  c (Result $ a - b)
equate (Expr Sub a (Result c)) (Result b) = equate  a (Result $ b + c)
equate (Expr Mul a (Result c)) (Result b) = equate  a (Result $ div b c)
equate (Expr Mul (Result a) c) (Result b) = equate  c (Result $ div b a)
equate (Expr Div a (Result c)) (Result b) = equate  a (Result $ b * c)
equate (Expr Div (Result a) c) (Result b) = equate  c (Result $ div a b)
equate (Var x) (Result b) = Equal (Var x) (Result b)


data Op = Add|Sub|Mul|Div deriving (Eq,Show)
data Expr = Result Integer | Var Text | Expr Op Expr Expr  |Equal Expr Expr
instance Show Expr where
    show (Result i) = show i
    show (Expr f a b) = mconcat ["(",show f," ",show a," ",show b,")"]
    show (Var x) = Text.unpack x
    show (Equal a b) = mconcat ["(= ",show a," ",show b,")"]

op "-" = Sub
op "+" = Add
op "*" = Mul
op "/" = Div

parseExpr :: Text -> (Text,Expr)
parseExpr input|any (`Text.elem` input) ['-','+','*','/'] = (\[monkey,a,b] -> (monkey,Expr (op (Text.filter (`Text.elem` "+-*/") input)) (Var a) (Var b))) . filter (not . Text.null) . Text.split (not . isLetter) $ input
                 |otherwise = (Text.takeWhile (/=':') input,Result . either (const 0) fst . Text.decimal . Text.takeWhile isNumber . Text.dropWhile (not . isNumber) $ input) 