{-# LANGUAGE OverloadedStrings #-}

module P06 where

import Data.Either
import Data.List
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
  print $ isBounded (0,0) [(x,y) | x <- [(-1),1], y <- [(-1),1]]
  print $ isBounded (0,0) [(1,1),(1,2),(-1,4)]

isBounded :: (Int, Int) -> [(Int, Int)] -> Bool
isBounded p ps =
  map (\p2 -> G.directionTo G.UnboundedSquareGrid p p2) ps
  |> concat
  |> nub
  |> length
  |> (== 4)
