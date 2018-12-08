{-# LANGUAGE OverloadedStrings #-}

module P07 where

import Util
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

type SleighStep = (Char, Char)
type SleighDict = M.Map Char [Char]
data WorkState = WS {
    time :: Int,
    workers :: Int,
    pending :: [(Char, Int)],
    done :: [Char],
    remaining :: [Char]
  } deriving (Show)

p07 :: IO ()
p07 = do
  input <- lines <$> slurp 7
  let dependencies = rights $ map (runParser parseSleighStep "") input
      allKeys = map fst dependencies ++ map snd dependencies |> sort |> nub
      backwardsMap = -- given a key, what steps must happen first?
        allKeys
        |> map (\k -> (k, map fst $ filter ((== k) . snd) dependencies))
        |> M.fromList
      start =
        allKeys
        |> map (\k -> (k, M.lookup k backwardsMap))
        |> filter ((== Just "") . snd)
        |> head
        |> fst
      part1 = walk (const 1) backwardsMap 1 allKeys start
      test2 = walk coster2test backwardsMap 2 allKeys start -- 253 < correct answer
      part2 = walk coster2 backwardsMap 5 allKeys start -- 253 < correct answer
              |> time
  putStr "part 1: "
  print part1
  putStrLn ""
  putStr "part 2 test: "
  print test2
  putStrLn ""
  putStr "part 2: "
  print part2

coster2test :: Char -> Int
coster2test=
  let m = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]
          |> M.fromList
  in (\c -> M.lookup c m |> fromJust)

coster2 :: Char -> Int
coster2 =
  let m = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [61..]
          |> M.fromList
  in (\c -> M.lookup c m |> fromJust)

-- walk :: (Char -> Int) -> SleighDict -> Int -> [Char] -> Char -> (String, Int)
walk coster g numWorkers allKeys start =
  iterate (step coster g)
          WS { time = 0, workers = numWorkers, pending = [(start, coster start)], done = [], remaining = (allKeys \\ [start]) }
  |> takeWhile (\ws -> not $ ((null $ remaining ws) && (null $ pending ws)))
  |> last

step :: (Char -> Int) -> SleighDict -> WorkState -> WorkState
step coster g workState =
  if (null $ remaining workState) && (null $ pending workState)
  then workState
  else
    let (finishedNow, ongoing) = partition (\(_,t) -> t <= time workState) (pending workState)
        ready = (map fst finishedNow) ++ (done workState)
        nexts = remaining workState
                |> filter (\r -> all ((flip elem) ready) (fromJust $ M.lookup r g)) -- only those whose precursors are all in pending
                |> sort
                |> take ((workers workState) - length ongoing)
                |> map (\k -> (k, (time workState) + (coster k)))
        inProgress = ongoing ++ nexts
        tick = inProgress
               |> map snd
               |> minimum
    in WS {
            time = tick,
            workers = workers workState,
            done = ready,
            pending = inProgress,
            remaining = (remaining workState) \\ (map fst nexts)
          }

takeUntil _ [] = []
takeUntil p (a:as) =
  if p a
  then (a : takeUntil p as)
  else [a]

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
