{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.String
import Data.Void
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

slurp :: IsString a => Int -> IO a
slurp i = do
  let filename = printf "inputs/%02d.txt" i
  contents <- readFile filename
  return $ fromString contents

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
