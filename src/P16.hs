{-# LANGUAGE OverloadedStrings #-}

module P16 where

import Util
import Data.Bits ((.&.),(.|.))
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Array.Unboxed as A
import Text.Megaparsec
import Text.Megaparsec.Char

data Sample = Sample { before :: [Int], after :: [Int], opcode :: Int, instruction :: (Int, Int, Int) }
  deriving (Eq, Show)

p16 :: IO ()
p16 = do
  input <- slurp 16
  let samples = fromRight [] $ runParser parseSamples "" input
      results = map testSample samples
      byOpcode = samples
                 |> sortBy (\s1 s2 -> compare (opcode s1) (opcode s2))
                 |> groupBy (\s1 s2 -> (opcode s1) == (opcode s2))
      faith = byOpcode
              |> fmap testOpcode
  putStrLn $ "part 1: " <> (show $ length $ filter (\os -> length os >= 3) results)
  print $ length byOpcode
  -- print byOpcode

testOpcode ss = -- given a set of samples, see which instructions are true for all samples
  instructions
  |> zip [0..]
  |> map (\(idx,i) -> (idx, all (\s -> i (before s) (instruction s) == (after s)) ss)) -- (idx, bool)
  |> filter snd
  |> map fst
  |> (\x -> (instsByPos M.! (opcode $ head ss), x))

testSample2 i s = (i (before s) (instruction s)) == (after s)

testSample s =
  map (\i -> i (before s) (instruction s)) instructions
  |> zip [0,1..]
  |> filter (\(_,o) -> o == (after s))

splice :: [Int] -> Int -> Int -> [Int]
splice regs reg val =
  (take reg regs) ++ [val] ++ (drop (reg + 1) regs)

-------------- instructions -------------------
instructions = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

instsByPos = zip [0..] [gtrr, eqri, bori, muli, gtir, eqrr, bani, borr, addr, seti, eqir, mulr, setr, banr, gtri, addi]
             |> M.fromList

addr regs (a,b,c) = (regs !! a) + (regs !! b) |> splice regs c
addi regs (a,b,c) = (regs !! a) + b |> splice regs c

mulr regs (a,b,c) = (regs !! a) * (regs !! b) |> splice regs c
muli regs (a,b,c) = (regs !! a) * b |> splice regs c

banr regs (a,b,c) = (regs !! a) .&. (regs !! b) |> splice regs c
bani regs (a,b,c) = (regs !! a) .&. b |> splice regs c

borr regs (a,b,c) = (regs !! a) .|. (regs !! b) |> splice regs c
bori regs (a,b,c) = (regs !! a) .|. b |> splice regs c

setr regs (a,_,c) = (regs !! a) |> splice regs c
seti regs (a,_,c) = a |> splice regs c

gtir regs (a,b,c) = (if a > (regs !! b) then 1 else 0) |> splice regs c
gtri regs (a,b,c) = (if (regs !! a) > b then 1 else 0) |> splice regs c
gtrr regs (a,b,c) = (if (regs !! a) > (regs !! b) then 1 else 0) |> splice regs c

eqir regs (a,b,c) = (if a == (regs !! b) then 1 else 0) |> splice regs c
eqri regs (a,b,c) = (if (regs !! a) == b then 1 else 0) |> splice regs c
eqrr regs (a,b,c) = (if (regs !! a) == (regs !! b) then 1 else 0) |> splice regs c

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
  return $ Sample { before = before, after = after, opcode = i, instruction = (a,b,c) }

parseSamples :: Parser [Sample]
parseSamples = endBy parseSample (optional newline)
