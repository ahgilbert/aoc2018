{-# LANGUAGE OverloadedStrings #-}

module P15 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p15 :: IO ()
p15 = do
  input <- lines <$> slurp 15
  mapM_ putStrLn input
