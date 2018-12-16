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
  print $ length samples

splice :: [Int] -> Int -> Int -> [Int]
splice regs reg val =
  (take reg regs) ++ [val] ++ (drop (reg + 1) regs)

addr :: [Int] -> (Int, Int, Int, Int) -> [Int]
addr regs (_,a,b,c) = (regs !! a) + (regs !! b) |> splice regs c
addi :: [Int] -> (Int, Int, Int, Int) -> [Int]
addi regs (_,a,b,c) = (regs !! a) + b |> splice regs c

mulr :: [Int] -> (Int, Int, Int, Int) -> [Int]
mulr regs (_,a,b,c) = (regs !! a) * (regs !! b) |> splice regs c
muli :: [Int] -> (Int, Int, Int, Int) -> [Int]
muli regs (_,a,b,c) = (regs !! a) * b |> splice regs c

banr :: [Int] -> (Int, Int, Int, Int) -> [Int]
banr regs (_,a,b,c) = (regs !! a) .&. (regs !! b) |> splice regs c
bani :: [Int] -> (Int, Int, Int, Int) -> [Int]
bani regs (_,a,b,c) = (regs !! a) .&. b |> splice regs c

borr :: [Int] -> (Int, Int, Int, Int) -> [Int]
borr regs (_,a,b,c) = (regs !! a) .|. (regs !! b) |> splice regs c
bori :: [Int] -> (Int, Int, Int, Int) -> [Int]
bori regs (_,a,b,c) = (regs !! a) .|. b |> splice regs c

setr :: [Int] -> (Int, Int, Int, Int) -> [Int]
setr regs (_,a,_,c) = (regs !! a) |> splice regs c
seti :: [Int] -> (Int, Int, Int, Int) -> [Int]
seti regs (_,a,_,c) = a |> splice regs c

gtir :: [Int] -> (Int, Int, Int, Int) -> [Int]
gtir regs (_,a,b,c) = (if a > (regs !! b) then 1 else 0) |> splice regs c
gtri :: [Int] -> (Int, Int, Int, Int) -> [Int]
gtri regs (_,a,b,c) = (if (regs !! a) > b then 1 else 0) |> splice regs c
gtrr :: [Int] -> (Int, Int, Int, Int) -> [Int]
gtrr regs (_,a,b,c) = (if (regs !! a) > (regs !! b) then 1 else 0) |> splice regs c

eqir :: [Int] -> (Int, Int, Int, Int) -> [Int]
eqir regs (_,a,b,c) = (if a == (regs !! b) then 1 else 0) |> splice regs c
eqri :: [Int] -> (Int, Int, Int, Int) -> [Int]
eqri regs (_,a,b,c) = (if (regs !! a) == b then 1 else 0) |> splice regs c
eqrr :: [Int] -> (Int, Int, Int, Int) -> [Int]
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
