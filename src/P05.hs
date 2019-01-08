{-# LANGUAGE OverloadedStrings #-}

module P05 where

import Util
import Data.Char
import Data.Either
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

-- TODO: On reduce, take a char blacklist whose contents are always removed
p05 :: IO ()
p05 = do
  input <- slurp 5
  let polymer = fromRight [] $ (runParser parsePolymer "") input
  print $ "Part 1: " <> (show $ length $ showPolymer $ zipReduce '0' polymer)
  let allChars = nub $ map c polymer
  let allReductions = map (\c -> zipReduce c polymer) allChars  -- (length . zipReduce) allSeeds
  let bestReduction = allReductions
                      |> map length
                      |> minimum
                      |> show
  print $ "Part 2: " <> bestReduction

matches :: Char -> Atom -> Bool
matches ch a = ch == c a

data Polarity = Up | Down
  deriving (Show, Eq)

data Atom = Atom { c :: Char, p :: Polarity }
  deriving (Show, Eq)

type Zipper a = ([a],[a])

mkZipper :: [a] -> Zipper a
mkZipper as = ([],as)

zipReduce :: Char -> [Atom] -> [Atom]
zipReduce nogo as =
  mkZipper as
  |> reduceGo nogo
  |> fst
  |> reverse

reduceGo :: Char -> Zipper Atom -> Zipper Atom
reduceGo nogo ((a:as),(b:bs)) =
  if c b == nogo
  then reduceGo nogo ((a:as),bs)
  else if c a == nogo
  then reduceGo nogo (as,b:bs)
  else if isCollision a b
  then reduceGo nogo (as,bs)
  else reduceGo nogo ((b:a:as),bs)
reduceGo _ (as,[]) = (as,[])
reduceGo nogo ([],(a:bs)) = reduceGo nogo ([a],bs)

isCollision :: Atom -> Atom -> Bool
isCollision a b = (toLower $ c a) == (toLower $ c b) && (p a) /= (p b)

------------- parsers ---------------
parseUp :: Parser Atom
parseUp = do
  c <- upperChar
  return Atom { c = toLower c, p = Up }

parseDown :: Parser Atom
parseDown = do
  c <- lowerChar
  return Atom { c = toLower c, p = Down }

parseAtom :: Parser Atom
parseAtom = choice [parseUp, parseDown]

parsePolymer :: Parser [Atom]
parsePolymer = many parseAtom

showPolymer :: [Atom] -> String
showPolymer as = map (\a -> if p a == Up then toUpper (c a) else c a) as
