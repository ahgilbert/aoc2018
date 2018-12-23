{-# LANGUAGE OverloadedStrings #-}

module P22 where

import Util
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

depth = 8787
target = (10,725)

data MazeType = Rocky | Narrow | Wet
type ModeMaze = State (M.Map Point Int)

p22 :: IO ()
p22 = do
  print (depth, target)

getOrUpdate :: (Ord k) => k -> State (M.Map k v) v -> State (M.Map k v) v
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (M.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (M.insert k ifEmpty)
      return ifEmpty

-- todo: memoize
geoIndex p@(x,y)
  | p == target = return 0
  | y == 0 = return $ x * 16807
  | x == 0 = return $ y * 48271
  | True = do
    l <- getOrUpdate (x-1,y) (geoIndex (x-1,y))
    u <- getOrUpdate (x,y-1) (geoIndex (x,y-1))
    return (l * u)
