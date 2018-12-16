{-# LANGUAGE OverloadedStrings #-}

module P16 where

import Util
import Data.Either
import qualified Data.Array.Unboxed as A
import Text.Megaparsec
import Text.Megaparsec.Char

data Sample = Sample { before :: [Int], after :: [Int], instruction :: (Int, Int, Int, Int) }
  deriving (Eq, Show)

emptySample = Sample { before = [], after = [], instruction = (0,0,0,0) }

p16 :: IO ()
p16 = do
  input <- slurp 16
  let samples = fromRight [emptySample] $ runParser parseSamples "" input
  print $ length samples

------------ parsers ---------------
parseSample :: Parser Sample
parseSample = do
  string "Before: "
  before <- read <$> manyTill asciiChar newline
  i <- parseInt
  space
  a <- parseInt
  space
  b <- parseInt
  space
  c <- parseInt
  newline
  string "After: "
  after <- read <$> manyTill asciiChar newline
  return $ Sample { before = before, after = after, instruction = (i,a,b,c) }

parseSamples :: Parser [Sample]
parseSamples = endBy parseSample (optional newline)
