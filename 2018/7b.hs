import Day7

import Data.Char
import Data.List 
import Data.Maybe

data Task = Task { taskStep :: Step, remaining :: Int } deriving (Show, Eq, Ord)
data Workshop = Workshop { workers :: Int, conds :: [Condition], steps :: [Step], tasks :: [Task] } deriving (Show)

fromStep :: Step -> Task
fromStep s = Task s (duration s)

tick :: Task -> Task
tick (Task s t) = Task s (t-1)

isDone :: Task -> Bool
isDone = (== 0) . remaining

duration :: Step -> Int
duration s = (ord s) - (ord 'A') + 61

workIsDone :: Workshop -> Bool
workIsDone (Workshop _ _ ss ts) = null ts && null ss

assignTasks :: Workshop -> Workshop
assignTasks (Workshop w cs ss ts)
    | isJust next = let s = fromJust next in assignTasks $ Workshop w cs (filter (/=s) ss) (fromStep s : ts)
    | otherwise = Workshop w cs ss ts
    where next = nextToRun cs ss

finishTasks :: Workshop -> Workshop
finishTasks (Workshop w cs ss ts) = Workshop w (filter (\c -> not $ elem (dep c) finished) cs) ss running
    where running = filter (not . isDone) ts
          finished = map taskStep $ filter isDone ts

doWork :: Workshop -> Workshop
doWork (Workshop w cs ss ts) = Workshop w cs ss (map tick ts)

totalDuration :: Workshop -> Int
totalDuration ws
    | workIsDone ws = 0
    | otherwise = 1 + (totalDuration $ finishTasks $ doWork $ assignTasks ws)

solve :: [Condition] -> Int
solve cs = totalDuration (Workshop 5 cs (allSteps cs) [])

main :: IO()
main = interact (show . solve . parse)

