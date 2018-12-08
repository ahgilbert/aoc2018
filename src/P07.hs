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
  -- print $ M.lookup start backwardsMap
      solution = walk backwardsMap allKeys start
  print $ solution

walk :: SleighDict -> [Char] -> Char -> [Char]
walk g allKeys start =
  step g ([start], (allKeys \\ [start]))
  |> fst
  |> reverse

step :: SleighDict -> ([Char], [Char]) -> ([Char], [Char])
step _ retVal@(_, []) = retVal
step g (soFar, remaining) =
  let next = remaining
             |> filter (\r -> all ((flip elem) soFar) (fromJust $ M.lookup r g)) -- only those whose precursers are all in soFar
             |> sort
             |> head
  in step g (next:soFar, remaining \\ [next])

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
