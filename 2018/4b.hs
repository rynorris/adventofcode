import Data.List 
import Data.List.Split

import Day4

solve events = head $ head $ sortBy (\l1 l2 -> compare (length l2) (length l1)) $ groupBy (==) $ sort $ sleepMinutes where
    sleepMinutes = foldl (++) [] $ map shiftSleepMinutes $ constructShifts $ sort $ events

main :: IO()
main = interact (show . solve . parse)

