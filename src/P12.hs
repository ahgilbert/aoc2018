{-# LANGUAGE OverloadedStrings #-}

module P12 where

import Util
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!?))
import Text.Megaparsec
import Text.Megaparsec.Char

type Cavern = M.Map Int Bool
type Rule = ([Bool], Bool)
type Rules = M.Map [Bool] Bool

p12 :: IO ()
p12 = do
  input <- slurp 12
  let (rules, cavern) = fromRight (M.empty,M.empty) $ runParser parseCavern "" input
      evol = iterate (generation rules) cavern
  print $ scoreCavern (evol !! 20)
  mapM_ print $ take 30 (map (\i -> (i, scoreCavern (evol !! i))) [100,200..])
  -- in the long run, growth is linear. derive formula by hand

scoreCavern :: Cavern -> Int
scoreCavern c =
  M.foldlWithKey' (\i k v -> if v then i + k else i) 0 c

generation :: Rules -> Cavern -> Cavern -- perform one generation
generation rs c =
  let leftmost = M.lookupMin c |> fromJust |> fst
      rightmost = M.lookupMax c |> fromJust |> fst
      bounds = [leftmost - 1..rightmost + 1]
  in map (grow rs c) bounds
     |> zip bounds
     |> M.fromAscList

grow :: Rules -> Cavern -> Int -> Bool
grow rs c idx =
  [idx - 2..idx + 2]
  |> map (\k -> maybe False id $ M.lookup k c)
  |> (\k -> M.lookup k rs)
  |> fromJust

printRule :: Rule -> String
printRule (conds, out) =
  let render b = if b then '#' else '.'
      left = map render conds
      right = render out
  in left ++ " => " ++ [right]

------------ parsers ---------------
parseCavern :: Parser (Rules, Cavern)
parseCavern = do
  string "initial state:"
  space
  cavern <- many parsePot
  spaceChar
  spaceChar
  rules <- many parseRule
  return (M.fromList rules, M.fromAscList (zip [0..] cavern))

parsePot :: Parser Bool
parsePot = (choice [(char '.' >> return False), (char '#' >> return True)])

parseRule :: Parser ([Bool], Bool)
parseRule = do
  pattern <- many parsePot
  string " => "
  result <- parsePot
  eol
  return (pattern, result)
