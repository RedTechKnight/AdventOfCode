{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Day25 where

import Data.Foldable
import Data.Text (Text (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

day25 :: IO ()
day25 = do
  input <- Text.readFile "inputs/day25"
  let snafu = Text.dropWhile (=='0') . foldl' (\acc x -> if Text.length acc > Text.length x then addSnafu acc x '0' else addSnafu x acc '0') "" . Text.lines $ input
  putStrLn . mconcat $ ["Day 25: ",Text.unpack snafu]

digitValue '2' = 2
digitValue '1' = 1
digitValue '0' = 0
digitValue '-' = -1
digitValue '=' = -2

fromSnafu = sum . zipWith (\i x -> x * (5^i)) [0..]. fmap digitValue . Text.unpack . Text.reverse

addSnafu :: Text -> Text -> Char -> Text
addSnafu "" "" carry = Text.singleton carry
addSnafu (Text.unsnoc -> Just (lhs,left)) "" carry = res sum where
  sum = digitValue left + digitValue carry
  res 3 = Text.snoc (addSnafu lhs "" '1') '='
  res 2 = Text.snoc lhs '2'
  res 1 = Text.snoc lhs '1'
  res 0 = Text.snoc lhs '0'
  res (-1) = Text.snoc lhs '-'
  res (-2) = Text.snoc lhs '='
  res (-3) = Text.snoc (addSnafu lhs "" '-') '2'
addSnafu (Text.unsnoc -> Just (lhs , left)) (Text.unsnoc -> Just (rhs,right)) carry = res sum where
  sum = digitValue left + digitValue right + digitValue carry
  res 0 = Text.snoc (addSnafu lhs rhs '0') '0'
  res 1 = Text.snoc (addSnafu lhs rhs '0') '1'
  res 2 = Text.snoc (addSnafu lhs rhs '0') '2'
  res 3 = Text.snoc (addSnafu lhs rhs '1') '='
  res 4 = Text.snoc (addSnafu lhs rhs '1') '-'
  res 5 = Text.snoc (addSnafu lhs rhs '1') '0'
  res (-1) = Text.snoc (addSnafu lhs rhs '0') '-'
  res (-2) =  Text.snoc (addSnafu lhs rhs '0') '='
  res (-3) =   Text.snoc (addSnafu lhs rhs '-') '2'
  res (-4) =  Text.snoc (addSnafu lhs rhs '-') '1'
  res (-5) = Text.snoc (addSnafu lhs rhs '-') '0'