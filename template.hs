{-# LANGUAGE OverloadedStrings #-}

module xxx where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

xxx :: IO ()
xxx = do
  input <- slurpLinesWith parseFaith xxx
  mapM_ putStrLn input

------------ parsers ---------------
parseFaith :: Parser String
parseFaith = do
  many anyChar
