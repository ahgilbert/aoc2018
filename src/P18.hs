{-# LANGUAGE OverloadedStrings #-}

module P18 where

import Util
import qualified Data.Array.Unboxed as A

type Settlers = A.Array Point Industry
data Industry = Open | Wooded | Lumberyard
  deriving (Eq, Show)

p18 :: IO ()
p18 = do
  settlers <- parseSettlers <$> lines <$> slurp 18
  mapM_ print settlers

------------ parsers ---------------

parseIndustry '.' = Open
parseIndustry '|' = Wooded
parseIndustry '#' = Lumberyard
parseIndustry  _  = undefined

parseSettlers :: [String] -> Settlers
parseSettlers rs = A.array ((0,0),(h-1,w-1)) assocs
  where
    w      = length (head rs)
    h      = length rs
    assocs = [((y,x), parseIndustry c) | (y,r) <- zip [0..] rs, (x,c) <- zip [0..] r]
