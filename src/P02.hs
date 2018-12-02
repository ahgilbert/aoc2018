{-# LANGUAGE OverloadedStrings #-}

module P02 where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char

p02 :: IO ()
p02 = do
  file <- lines <$> slurp 2
  mapM_ print file
