{-# LANGUAGE OverloadedStrings #-}

module P16 where

import Util
import Data.Bits ((.&.),(.|.))
import Data.Either
import qualified Data.Array.Unboxed as A
import Text.Megaparsec
import Text.Megaparsec.Char

data Sample = Sample { before :: [Int], after :: [Int], instruction :: (Int, Int, Int, Int) }
  deriving (Eq, Show)

p16 :: IO ()
p16 = do
  input <- slurp 16
  let samples = fromRight [] $ runParser parseSamples "" input
      results = map testSample samples
                |> filter (\os -> length os >= 3)
  print $ length results

testSample s =
  map (\i -> i (before s) (instruction s)) instructions
  |> zip [0,1..]
  |> filter (\(_,o) -> o == (after s))

splice :: [Int] -> Int -> Int -> [Int]
splice regs reg val =
  (take reg regs) ++ [val] ++ (drop (reg + 1) regs)

-------------- instructions -------------------
instructions = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

addr regs (_,a,b,c) = (regs !! a) + (regs !! b) |> splice regs c
addi regs (_,a,b,c) = (regs !! a) + b |> splice regs c

mulr regs (_,a,b,c) = (regs !! a) * (regs !! b) |> splice regs c
muli regs (_,a,b,c) = (regs !! a) * b |> splice regs c

banr regs (_,a,b,c) = (regs !! a) .&. (regs !! b) |> splice regs c
bani regs (_,a,b,c) = (regs !! a) .&. b |> splice regs c

borr regs (_,a,b,c) = (regs !! a) .|. (regs !! b) |> splice regs c
bori regs (_,a,b,c) = (regs !! a) .|. b |> splice regs c

setr regs (_,a,_,c) = (regs !! a) |> splice regs c
seti regs (_,a,_,c) = a |> splice regs c

gtir regs (_,a,b,c) = (if a > (regs !! b) then 1 else 0) |> splice regs c
gtri regs (_,a,b,c) = (if (regs !! a) > b then 1 else 0) |> splice regs c
gtrr regs (_,a,b,c) = (if (regs !! a) > (regs !! b) then 1 else 0) |> splice regs c

eqir regs (_,a,b,c) = (if a == (regs !! b) then 1 else 0) |> splice regs c
eqri regs (_,a,b,c) = (if (regs !! a) == b then 1 else 0) |> splice regs c
eqrr regs (_,a,b,c) = (if (regs !! a) == (regs !! b) then 1 else 0) |> splice regs c

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
