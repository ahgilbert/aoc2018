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
  let (samples, prog) = fromRight ([],[]) $ runParser parseChronalClass "" input
      results = map testSample samples
      byOpcode = samples
                 |> sortBy (\s1 s2 -> compare (opcode s1) (opcode s2))
                 |> groupBy (\s1 s2 -> (opcode s1) == (opcode s2))
                 |> map (\ss -> ((opcode $ head ss), ss))
      faith = byOpcode
              |> (map . fmap) testOpcode
      hope = foldl (runcmd instsByOpcode) [0,0,0,0] $ prog
  putStrLn $ "part 1: " <> (show $ length $ filter (\os -> length os >= 3) results)
  mapM_ print faith
  print $ length prog
  print hope

runcmd :: M.Map Int ([Int] -> (Int, Int, Int) -> [Int]) -> [Int] -> (Int, (Int, Int, Int)) -> [Int]
runcmd insts regs (i,args) = (insts M.! i) regs args

instructions =
  zip
    ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"]
    [ addr,   addi,   mulr,   muli,   banr,   bani,   borr,   bori,   setr,   seti,   gtir,   gtri,   gtrr,   eqir,   eqri,   eqrr]

-- by hand, sudoku style
instsByOpcode = [(0,setr),(1,eqrr),(2,gtri),(3,muli),(4,eqir),(5,borr),(6,bori),(7,mulr),(8,gtrr),(9,seti),(10,banr),(11,eqri),(12,addr),(13,gtir),(14,addi),(15,bani)]
                |> M.fromList

testOpcode :: [Sample] -> [String]
testOpcode ss = -- given a set of samples, see which instructions are true for all samples
  fmap (\(iname, i) -> (iname, all (\s -> i (before s) (instruction s) == (after s)) ss)) instructions
  |> filter snd
  |> map fst

testSample2 i s = (i (before s) (instruction s)) == (after s)

testSample s =
  (map . fmap) (\i -> i (before s) (instruction s)) instructions
  |> filter (\(_,o) -> o == (after s))

splice :: [Int] -> Int -> Int -> [Int]
splice regs reg val =
  (take reg regs) ++ [val] ++ (drop (reg + 1) regs)

-------------- instructions -------------------
addr regs (a,b,c) = (regs !! a) + (regs !! b) |> splice regs c
addi regs (a,b,c) = (regs !! a) + b           |> splice regs c

mulr regs (a,b,c) = (regs !! a) * (regs !! b) |> splice regs c
muli regs (a,b,c) = (regs !! a) * b           |> splice regs c

banr regs (a,b,c) = (regs !! a) .&. (regs !! b) |> splice regs c
bani regs (a,b,c) = (regs !! a) .&. b           |> splice regs c

borr regs (a,b,c) = (regs !! a) .|. (regs !! b) |> splice regs c
bori regs (a,b,c) = (regs !! a) .|. b           |> splice regs c

setr regs (a,_,c) = (regs !! a) |> splice regs c
seti regs (a,_,c) = a           |> splice regs c

gtir regs (a,b,c) = (if a           > (regs !! b) then 1 else 0) |> splice regs c
gtri regs (a,b,c) = (if (regs !! a) > b           then 1 else 0) |> splice regs c
gtrr regs (a,b,c) = (if (regs !! a) > (regs !! b) then 1 else 0) |> splice regs c

eqir regs (a,b,c) = (if a           == (regs !! b) then 1 else 0) |> splice regs c
eqri regs (a,b,c) = (if (regs !! a) == b           then 1 else 0) |> splice regs c
eqrr regs (a,b,c) = (if (regs !! a) == (regs !! b) then 1 else 0) |> splice regs c
------------ parsers ---------------
parseChronalClass :: Parser ([Sample], [(Int, (Int, Int, Int))])
parseChronalClass = do
  samples <- parseSamples
  many newline
  insts <- many parseInstruction
  return (samples, insts)

parseInstruction :: Parser (Int, (Int, Int, Int))
parseInstruction = do
  i <- parseInt
  space
  a <- parseInt
  space
  b <- parseInt
  space
  c <- parseInt
  newline
  return (i,(a,b,c))

parseSample :: Parser Sample
parseSample = do
  string "Before: "
  before <- read <$> manyTill asciiChar newline
  (i,(a,b,c)) <- parseInstruction
  string "After: "
  after <- read <$> manyTill asciiChar newline
  return $ Sample { before = before, after = after, opcode = i, instruction = (a,b,c) }

parseSamples :: Parser [Sample]
parseSamples = endBy parseSample (optional newline)
