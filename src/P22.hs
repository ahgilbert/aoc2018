{-# LANGUAGE OverloadedStrings #-}

module P22 where

import Util
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

depth = 8787
target = (10,725)

data MazeType = Rocky | Narrow | Wet
type ModeMaze = State (M.Map Point Int)

p22 :: IO ()
p22 = do
  print (depth, target)

-- todo: memoize
geoIndex p@(x,y)
  | p == target = 0
  | y == 0 = x * 16807
  | x == 0 = y * 48271
  | True = geoIndex (x-1,y) * geoIndex (x,y-1)

memoFib :: Int -> Integer
memoFib = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = memoFib (n - 2) + memoFib (n - 1)
