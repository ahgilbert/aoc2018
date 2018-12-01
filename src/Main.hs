{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import Data.Either
import qualified Data.Set as S
import Util
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

main :: IO ()
main = do
  file <- lines <$> slurp 1
  let nums = rights $ map (runParser parseInt "") file
  let p1 = foldr (+) 0 nums
  print p1
  let nums2 = cycle nums
  let p2 = scanl (+) 0 nums2
  print "ahg"

folder :: (Ord a) => S.Set a -> a -> (Bool, S.Set a)
folder s x
  | S.member x s = (True, s)
  | otherwise = (False, S.insert x s)

parsePosInt :: Parser Int
parsePosInt = do
  string "+"
  digits <- many digitChar
  return $ read digits

parseNegInt :: Parser Int
parseNegInt = do
  string "-"
  digits <- many digitChar
  return $ 0 - (read digits)

parseInt :: Parser Int
parseInt = choice [parseNegInt, parsePosInt]
