{-# LANGUAGE OverloadedStrings #-}

module P09 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p09 :: IO ()
p09 = do
  faith <- slurpLinesWith parseFaith 9
  print "write some code"


------------ parsers ---------------
parseFaith :: Parser String
parseFaith = do
  many anyChar
