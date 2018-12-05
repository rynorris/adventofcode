import Data.List 
import Data.List.Split

import Day4

solve events = chosenMinute * sleepiestGuard where
    chosenMinute = head $ head $ reverse $ sortBy (\l1 l2 -> compare (length l1) (length l2)) $ groupBy (==) $ sort $ map minuteMinute sleepiestGuardMinutes where
    sleepiestGuardMinutes = filter (\m -> (minuteGuard m) == sleepiestGuard) sleepMinutes
    sleepiestGuard = head $ head $ sortBy (\l1 l2 -> compare (length l2) (length l1)) $ groupBy (==) $ sort $ map minuteGuard sleepMinutes
    sleepMinutes = foldl (++) [] $ map shiftSleepMinutes $ constructShifts $ sort $ events

main :: IO()
main = interact (show . solve . parse)
