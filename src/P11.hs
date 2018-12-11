{-# LANGUAGE OverloadedStrings #-}

module P11 where

import Util
import System.Environment

p11 :: IO ()
p11 = do
  serial <- getArgs
  print serial

