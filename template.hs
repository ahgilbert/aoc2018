{-# LANGUAGE OverloadedStrings #-}

module xxx where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

xxx :: IO ()
xxx = do
  faith <- slurpLinesWith parseFaith xxx
  print "write some code"


------------ parsers ---------------
parseFaith :: Parser String
parseFaith = do
  many anyChar
