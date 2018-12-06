{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.String
import Data.Void
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) = flip (<$>)
infixl 4 ||>

slurp :: IsString a => Int -> IO a
slurp i = do
  let filename = printf "inputs/%02d.txt" i
  contents <- readFile filename
  return $ fromString contents

uniquePairs :: Ord a => [a] -> [(a,a)]
uniquePairs xs = [(a,b) | a <- xs, b <- xs, a < b]

parsePosInt :: Parser Int
parsePosInt = do
  optional $ string "+"
  digits <- many digitChar
  return $ read digits

parseNegInt :: Parser Int
parseNegInt = do
  string "-"
  digits <- many digitChar
  return $ 0 - (read digits)

parseInt :: Parser Int
parseInt = choice [parseNegInt, parsePosInt]

parsePoint :: Parser (Int, Int)
parsePoint = do
  x <- parseInt
  char ','
  space
  y <- parseInt
  return (x,y)
