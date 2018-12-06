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
  let reduced = showPolymer $ zipReduce polymer
  print $ length reduced
  let allChars = nub $ map (toLower . c) polymer
  let allSeeds = map (\ch -> filter (not . (matches ch)) polymer) allChars
  let allReductions = map (length . zipReduce) allSeeds
  let bestReduction = minimum allReductions
  print bestReduction

matches :: Char -> Atom -> Bool
matches ch a = ch == (toLower $ c a)

data Polarity = Up | Down
  deriving (Show, Eq)

data Atom = Atom { c :: Char, p :: Polarity }
  deriving (Show, Eq)

type Zipper a = ([a],[a])

mkZipper :: [a] -> Zipper a
mkZipper as = ([],as)

zipDone (_,[]) = True
zipDone _ = False

zipStep :: Zipper a -> Zipper a
zipStep (as,[]) = (as,[])
zipStep (as,(b:bs)) = (b:as, bs)

zipReduce :: [Atom] -> [Atom]
zipReduce as =
  mkZipper as
  |> reduceGo
  |> fst
  |> reverse

reduceGo :: Zipper Atom -> Zipper Atom
reduceGo ((a:as),(b:bs)) =
  if isCollision a b
  then reduceGo (as,bs)
  else reduceGo ((b:a:as),bs)
reduceGo (as,[]) = (as,[])
reduceGo ([],(a:bs)) = reduceGo ([a],bs)

isCollision :: Atom -> Atom -> Bool
isCollision a b = (toLower $ c a) == (toLower $ c b) && (p a) /= (p b)

------------- parsers ---------------
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
