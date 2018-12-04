{-# LANGUAGE OverloadedStrings #-}

module P04 where

import Util
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

p04 :: IO ()
p04 = do
  input <- sort <$> lines <$> slurp 4
  let log = rights $ map (runParser parseShiftEvent "") input
      naps = sumSleeps log
  let allNaps = M.fromListWith (++) naps
      lazyGuard = fst $ maximumBy (\(_,a) (_,b) -> compare a b) $ M.toList $ fmap length allNaps
      napMinutes = fromJust $ fmap (group . sort) (M.lookup lazyGuard allNaps)
      laziestMinute = head $ maximumBy (\a b -> compare (length a) (length b)) $ napMinutes
  print lazyGuard
  print laziestMinute
  print $ lazyGuard * laziestMinute

type LogEntry = (Timestamp, ShiftEvent)

data ShiftEvent =
    BeginShift Int
  | FallsAsleep
  | WakesUp
  deriving (Show)

data Timestamp = TS { y :: Int, mo :: Int, d :: Int, h :: Int, mi :: Int }
  deriving (Show)

getGuard (_, (BeginShift x)) = x
getGuard _ = undefined

sumSleeps :: [LogEntry] -> [(Int, [Int])]
sumSleeps [] = []
sumSleeps log =
  let shift = sumSleep log
  in (shift:(sumSleeps $ dropWhile (not . newShift . snd) (tail log)))

sumSleep :: [LogEntry] -> (Int, [Int])
sumSleep log =
  let guard = getGuard (head log)
      thisShift = takeWhile (not . newShift . snd) (tail log)
      minutesSlept = calcShift thisShift
  in (guard, minutesSlept)

calcShift :: [LogEntry] -> [Int]
calcShift [] = []
calcShift (begin:end:es) =
  let span = [(mi (fst begin))..(mi (fst end) - 1)]
  in span ++ (calcShift es)

newShift (BeginShift _) = True
newShift _ = False

------------- parsers ------------------

parseShiftEvent :: Parser LogEntry
parseShiftEvent = do
  time <- parseTimestamp
  event <- choice [parseWakesUp,
                   parseFallsAsleep,
                   parseShiftStart]
  return (time, event)

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
  string "Guard #"
  guardId <- read <$> some digitChar
  space
  string "begins shift"
  return (BeginShift guardId)

