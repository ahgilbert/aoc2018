{-# LANGUAGE OverloadedStrings #-}

module Main where

import Util

main :: IO ()
main = do
  file <- slurp 3
  putStrLn file
