{-# LANGUAGE OverloadedStrings #-}

module P12 where

import Util
import Data.Either
import Data.List
import qualified Data.Array as A
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

type Cavern = A.Array Bool
type Rule = ([Bool], Bool)
type Rules = M.Map [Bool] Bool

p12 :: IO ()
p12 = do
  input <- slurp 12
  let (rules, cavern) = fromRight ([],[]) $ runParser parseCavern "" input
  print "ahg" 

printRule :: Rule -> String
printRule (conds, out) =
  let render b = if b then '#' else '.'
      left = map render conds
      right = render out
  in left ++ " => " ++ [right]

------------ parsers ---------------
parseCavern :: Parser ([Rule], Cavern)
parseCavern = do
  string "initial state:"
  space
  cavern <- many parsePot
  spaceChar
  spaceChar
  rules <- many parseRule
  return (rules, cavern)

parsePot :: Parser Bool
parsePot = (choice [(char '.' >> return False), (char '#' >> return True)])

parseRule :: Parser ([Bool], Bool)
parseRule = do
  pattern <- many parsePot
  string " => "
  result <- parsePot
  eol
  return (pattern, result)
