{-# LANGUAGE OverloadedStrings #-}

module P11 where

import Util
import System.Environment

p11 :: IO ()
p11 = do
  serial <- getArgs ||> head ||> read
  print (0 + serial)

powerLevel :: Int -> Point -> Int
powerLevel serial (x,y) =
  let rack = x + 10
  in ((rack * y) + serial) * rack
     |> hundreds
     |> (\i -> i - 5)

hundreds :: Int -> Int -- return hundreds digit
hundreds n =
  ((n `mod` 1000) - (n `mod` 100)) `div` 100

