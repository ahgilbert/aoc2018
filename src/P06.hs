{-# LANGUAGE OverloadedStrings #-}

module P06 where

import Data.Either
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.Grid.Square as G
import Text.Megaparsec
import Text.Megaparsec.Char
import Util

p06 :: IO ()
p06 = do
  input <- lines <$> slurp 6
  let points = rights $ map (runParser parsePoint "") input
      g = G.UnboundedSquareGrid
  mapM_ print points
  print $ G.distance g (0,0) (0,1)
