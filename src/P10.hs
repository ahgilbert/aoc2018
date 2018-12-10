{-# LANGUAGE OverloadedStrings #-}

module P10 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p10 :: IO ()
p10 = do
  faith <- slurpLinesWith parseFaith 10
  print "write some code"


------------ parsers ---------------
parseFaith :: Parser String
parseFaith = do
  many anyChar
