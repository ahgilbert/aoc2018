{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.String
import Text.Printf

slurp :: IsString a => Int -> IO a
slurp i = do
  let filename = printf "inputs/%02d.txt" i
  contents <- readFile filename
  return $ fromString contents
