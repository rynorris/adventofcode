module Day4 where 

import Common

import Data.List 
import Data.List.Split

data Timestamp = Timestamp Int Int Int Int Int deriving (Show, Eq, Ord)
data Action = Begin Int | Sleep | Wake deriving (Show, Eq, Ord)
data Event = Event Timestamp Action deriving (Show, Eq, Ord)
data Shift = Shift Int [Event] deriving (Show, Eq, Ord)
data SleepMinute = SleepMinute Int Int deriving(Show, Eq, Ord)

shiftGuard :: Shift -> Int
shiftGuard (Shift id _) = id

minuteGuard :: SleepMinute -> Int
minuteGuard (SleepMinute id _) = id

minuteMinute :: SleepMinute -> Int
minuteMinute (SleepMinute _ m) = m

isBegin :: Event -> Bool
isBegin (Event _ (Begin _)) = True
isBegin _ = False

constructTimestamp :: [Int] -> Timestamp
constructTimestamp xs = Timestamp (xs!!0) (xs!!1) (xs!!2) (xs!!3) (xs!!4)

constructAction :: [String] -> Action
constructAction ["falls", _] = Sleep
constructAction ["wakes", _] = Wake
constructAction xs = Begin (read $ remove "#" (xs!!1))

constructShift :: [Event] -> Shift
constructShift ((Event _ (Begin x)):es) = Shift x es
constructShift _ = Shift 0 []

constructShifts :: [Event] -> [Shift]
constructShifts events = map constructShift $ (split $ keepDelimsL $ whenElt isBegin) events

sleepMinutes :: Int -> Event -> Event -> [SleepMinute]
sleepMinutes id (Event (Timestamp _ _ _ _ m1) Sleep) (Event (Timestamp _ _ _ _ m2) Wake) = [SleepMinute id m | m <- [m1..(m2-1)]]
sleepMinutes _ _ _ = []

shiftSleepMinutes :: Shift -> [SleepMinute]
shiftSleepMinutes (Shift id es) = foldl (++) [] $ map (uncurry (sleepMinutes id)) $ zip es (drop 1 es)

parseLine s = Event timestamp action where
    timestamp = constructTimestamp $ map read $ take 5 split
    action = constructAction $ drop 5 split
    split = words $ replace "[]-:" ' ' s

parse :: String -> [Event]
parse = (map parseLine) . lines
