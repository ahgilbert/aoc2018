{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad
import Data.String
import Data.Either
import Data.Void
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String
type Point = (Int, Int)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) = flip (<$>)
infixl 4 ||>

slurpLinesWith :: Parser a -> Int -> IO [a]
slurpLinesWith parser i =
  rights . map (runParser parser "") . lines <$> readFile (printf "inputs/%02d.txt" i)

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

north = (0,-1)
west  = (-1,0)
east  = (1, 0)
south = (0, 1)

neighbors4 :: Point -> [Point] -- get NWES
neighbors4 p = map (addPoints p) [north, west, east, south]

addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

parsePoint :: Parser Point
parsePoint = do
  x <- parseInt
  char ','
  space
  y <- parseInt
  return (x,y)
