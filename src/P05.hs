{-# LANGUAGE OverloadedStrings #-}

module P05 where

import Util
import Data.Char
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

p05 :: IO ()
p05 = do
  -- input <- slurp 5
  let input = "dabAcCaCBAcCcaDA"
  let polymer = fromRight [] $ (runParser parsePolymer "") input
  let final = reduce polymer
  print $ showPolymer final

data Polarity = Up | Down
  deriving (Show, Eq)

data Atom = Atom { c :: Char, p :: Polarity }
  deriving (Show, Eq)

showPolymer :: [Atom] -> String
showPolymer as = map c as

reduce :: [Atom] -> [Atom]
reduce [] = []
reduce (a:[]) = [a]
reduce (a:b:cs) =
  if isCollision a b
  then reduce cs
  else (a:(reduce (b:cs)))

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
