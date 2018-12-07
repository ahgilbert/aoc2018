{-# LANGUAGE OverloadedStrings #-}

module P06 where

import Data.Either
import Data.List
import Data.Maybe
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.Grid.Square as G
import Text.Megaparsec
import Text.Megaparsec.Char
import Util

grid = G.UnboundedSquareGrid

p06 :: IO ()
p06 = do
  input <- lines <$> slurp 6
  let points = rights $ map (runParser parsePoint "") input
      bottomLeft = (minimum (map fst points), minimum (map snd points))
      topRight = (maximum (map fst points), maximum (map snd points))
      bounds = enclosedSpace bottomLeft topRight
  let distancesToNearest = map (getNearest points) bounds
      voroni = distancesToNearest
               |> filter isJust
               |> map fromJust
               |> sort
               |> group
               |> map (\xx -> (head xx, length xx))
      winner = voroni
               |> maximumBy (\(_,a) (_,b) -> compare a b)
  putStrLn $ "Part 1: " <> show winner
  let faith = map (sumOfDistances points) bounds
      hope = faith
             |> map (< 10000)
             |> filter id
             |> length
  print hope

sumOfDistances :: [Point] -> Point -> Int
sumOfDistances targets origin =
  map (\p -> G.distance grid origin p) targets
  |> sum

enclosedSpace (xBL,yBL) (xTR,yTR) = [(x,y) | x <- [xBL..xTR], y <- [yBL..yTR]]

getNearest :: [Point] -> Point -> Maybe Point
getNearest ps p =
  let distances = map (\target -> (target, G.distance grid p target)) ps
      dToNearest = distances
                   |> minimumBy (\(_,a) (_,b) -> compare a b)
                   |> snd
  in filter (\(_,d) -> d == dToNearest) distances
     |> (\nearests ->
           if length nearests == 1
           then Just (fst $ head nearests)
           else Nothing)

isBounded :: Point -> [Point] -> Bool
isBounded p ps =
  map (\p2 -> G.directionTo grid p p2) ps
  |> concat
  |> nub
  |> length
  |> (== 4)
