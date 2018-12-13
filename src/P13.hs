{-# LANGUAGE OverloadedStrings #-}

module P13 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p13 :: IO ()
p13 = do
  input <- slurpLinesWith parseFaith 13
  mapM_ putStrLn input


------------ parsers ---------------
parseFaith :: Parser String
parseFaith = do
  many anyChar
