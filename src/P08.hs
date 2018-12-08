{-# LANGUAGE OverloadedStrings #-}

module P08 where

import Util
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

p08 :: IO ()
p08 = do
  input <- lines <$> slurp 8
  let license = rights $ map (runParser parseMemory "") input
  print "write some code"

----------- parsers -------------

parseIntToken :: Parser Int
parseIntToken = do
  i <- parseInt
  space
  return i

parseMemory :: Parser [Int]
parseMemory = do
  many parseIntToken
