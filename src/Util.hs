{-# LANGUAGE OverloadedStrings #-}

module Util where

import Text.Printf

slurp :: Int -> IO ()
slurp i = do
  let filename = printf "inputs/%02d.txt" i
  putStrLn filename
