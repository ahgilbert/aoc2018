{-# LANGUAGE OverloadedStrings #-}

module P04 where

import Util
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

p04 :: IO ()
p04 = do
  input <- sort <$> lines <$> slurp 104
  mapM_ print input

data ShiftEvent =
    BeginShift Int
  | FallsAsleep
  | WakesUp

data Timestamp = TS { y :: Int, mo :: Int, d :: Int, h :: Int, mi :: Int }

------------- parsers ------------------

parseShiftEvent :: Parser ShiftEvent
parseShiftEvent =
  choice [
    parseWakesUp,
    parseFallsAsleep,
    parseShiftStart
  ]

parseTimestamp :: Parser Timestamp
parseTimestamp = do
  stamp <- between (char '[') (char ']') parseDateTime
  space
  return stamp

parseDateTime :: Parser Timestamp
parseDateTime = do
  yy <- read <$> some digitChar
  char '-'
  mmo <- read <$> some digitChar
  char '-'
  dd <- read <$> some digitChar
  space
  hh <- read <$> some digitChar
  char ':'
  mmi <- read <$> some digitChar
  return TS { y = yy, mo = mmo, d = dd, h = hh, mi = mmi }

parseWakesUp :: Parser ShiftEvent
parseWakesUp = do
  string "wakes up"
  return WakesUp

parseFallsAsleep :: Parser ShiftEvent
parseFallsAsleep = do
  string "falls asleep"
  return FallsAsleep

parseShiftStart :: Parser ShiftEvent
parseShiftStart = do
  timestamp <- parseTimestamp
  string "Guard #"
  guardId <- read <$> some digitChar
  space
  string "begins shift"
  return (BeginShift guardId)

