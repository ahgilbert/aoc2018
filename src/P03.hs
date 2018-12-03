{-# LANGUAGE OverloadedStrings #-}

module P03 where

import Util
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

data Patch = Patch { ident :: Int, corner :: (Int, Int), size :: (Int, Int) }
  deriving (Show)

p03_1 = do
  input <- slurp 103 ||> lines
  let patches = rights $ map (runParser parsePatch "") input
  mapM_ print patches

p03_2 = do
  input <- slurp 3 ||> lines
  print "unimplemented"

parseUnsignedInt = some digitChar ||> read

parseCorner :: Parser (Int, Int)
parseCorner = do
  x <- parseUnsignedInt
  char ','
  y <- parseUnsignedInt
  return (x,y)

parseSize :: Parser (Int, Int)
parseSize = do
  x <- parseUnsignedInt
  char 'x'
  y <- parseUnsignedInt
  return (x,y)

parseId :: Parser Int
parseId = do
  char '#'
  parseUnsignedInt

parsePatch :: Parser Patch
parsePatch = do
  id <- parseId
  string " @ "
  corner <- parseCorner
  string ": "
  size <- parseSize
  return $ Patch { ident = id, corner = corner, size = size }
