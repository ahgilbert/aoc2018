{-# LANGUAGE OverloadedStrings #-}

module P06 where

import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import Util

p06 :: IO ()
p06 = do
  input <- lines <$> slurp 6
  let points = rights $ map (runParser parsePoint "") input
  mapM_ print points

