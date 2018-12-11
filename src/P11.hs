{-# LANGUAGE OverloadedStrings #-}

module P11 where

import Util
import Data.List
import System.Environment

p11 :: IO ()
p11 = do
  serial <- getArgs ||> head ||> read
  let candidates = [(x,y) | x <- [1..298], y <- [1..298]]
      levels = map (\p -> (p, regionPower serial 3 p)) candidates
      winner = maximumBy (\a b -> compare (snd a) (snd b)) levels
  print winner

regionPower :: Int -> Int -> Point -> Int -- power level of 3x3 with Point as top left
regionPower serial size (tlx,tly) =
  let s = size - 1
      neighborhood = [(x,y) | x <- [tlx..tlx + s], y <- [tly..tly + s]]
  in map (powerLevel serial) neighborhood
     |> sum

powerLevel :: Int -> Point -> Int
powerLevel serial (x,y) =
  let rack = x + 10
  in ((rack * y) + serial) * rack
     |> hundreds
     |> (\i -> i - 5)

hundreds :: Int -> Int -- return hundreds digit
hundreds n =
  ((n `mod` 1000) - (n `mod` 100)) `div` 100

