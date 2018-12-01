{-# LANGUAGE OverloadedStrings #-}

module P01 where

import Data.Void
import Data.Either
import qualified Data.Set as S
import Util
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

p01 :: IO ()
p01 = do
  file <- lines <$> slurp 1
  let nums = rights $ map (runParser parseInt "") file
  let p1 = foldr (+) 0 nums
  print $ p1
  let nums2 = cycle nums
  let frequencies = scanl (+) 0 nums2
  let p2s = tail $ scanl folder (True, 0, S.empty) (frequencies) -- tail to drop the seed val
  let p2 = snd3 $ head $ dropWhile (not . fst3) p2s
  print p2

fst3 (b,_,_) = b
snd3 (_,v,_) = v

folder :: (Ord a) => (Bool, a, S.Set a) -> a -> (Bool, a, S.Set a)
folder (_,_,s) x
  | S.member x s = (True, x, s)
  | otherwise = (False, x, S.insert x s)

parsePosInt :: Parser Int
parsePosInt = do
  string "+"
  digits <- many digitChar
  return $ read digits

parseNegInt :: Parser Int
parseNegInt = do
  string "-"
  digits <- many digitChar
  return $ 0 - (read digits)

parseInt :: Parser Int
parseInt = choice [parseNegInt, parsePosInt]
