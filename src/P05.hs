{-# LANGUAGE OverloadedStrings #-}

module P05 where

import Util
import Data.Char
import Data.Either
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

p05 :: IO ()
p05 = do
  input <- slurp 5
  let polymer = fromRight [] $ (runParser parsePolymer "") input
  let reduced = showPolymer $ reduce polymer
  print $ length reduced
  let allChars = nub $ map (toLower . c) polymer
  let allSeeds = map (\ch -> filter (not . (matches ch)) polymer) allChars
  let allReductions = map (length . reduce) allSeeds
  let bestReduction = minimum allReductions
  print bestReduction

matches :: Char -> Atom -> Bool
matches ch a = ch == (toLower $ c a)

data Polarity = Up | Down
  deriving (Show, Eq)

data Atom = Atom { c :: Char, p :: Polarity }
  deriving (Show, Eq)

reduce :: [Atom] -> [Atom]
reduce polymer =
  let reductions = iterate reduce' polymer
  in findFixed reductions

reduce' :: [Atom] -> [Atom]
reduce' [] = []
reduce' (a:[]) = [a]
reduce' (a:b:cs) =
  if isCollision a b
  then reduce' cs
  else (a:(reduce' (b:cs)))

findFixed :: [[a]] -> [a]
findFixed [] = []
findFixed (a:[]) = []
findFixed (a:b:cs) =
  if length a == length b
  then a
  else findFixed (b:cs)

isCollision :: Atom -> Atom -> Bool
isCollision a b = (toLower $ c a) == (toLower $ c b) && (p a) /= (p b)

stripCollision :: [Atom] -> [Atom]
stripCollision = undefined

parseUp :: Parser Atom
parseUp = do
  c <- upperChar
  return Atom { c = c, p = Up }

parseDown :: Parser Atom
parseDown = do
  c <- lowerChar
  return Atom { c = c, p = Down }

parseAtom :: Parser Atom
parseAtom = choice [parseUp, parseDown]

parsePolymer :: Parser [Atom]
parsePolymer = many parseAtom

showPolymer :: [Atom] -> String
showPolymer as = map c as
