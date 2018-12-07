import Data.Char
import Data.List 
import Data.Maybe

import Debug.Trace

type Step = Char
data Condition = Condition Step Step deriving (Show, Eq, Ord)

condDep :: Condition -> Step
condDep (Condition d _) = d

condStep :: Condition -> Step
condStep (Condition _ s) = s

parseCondition :: String -> Condition
parseCondition line = Condition (head (ws !! 1)) (head (ws !! 7)) where
    ws = words line

parse :: String -> [Condition]
parse = map parseCondition . lines

allSteps :: [Condition] -> [Step]
allSteps cs = nub $ sort $ (map condDep cs) ++ (map condStep cs)

canRun :: [Condition] -> Step -> Bool
canRun cs s = null $ filter (== s) $ map condStep cs

nextToRun :: [Condition] -> [Step] -> Maybe Step
nextToRun cs ss | null runnable  = Nothing
                | otherwise = Just (head runnable)
    where runnable = filter (canRun cs) ss

-- NEW

data Task = Task Step Int deriving (Show, Eq, Ord)

fromStep :: Step -> Task
fromStep s = Task s (duration s)

toStep :: Task -> Step
toStep (Task s _) = s

tick :: Task -> Task
tick (Task s t) = Task s (t-1)

isDone :: Task -> Bool
isDone (Task _ t) = t == 0

duration :: Step -> Int
duration s = (ord s) - (ord 'A') + 61

totalDuration :: Int -> [Condition] -> [Step] -> [Task] -> Int
totalDuration w cs ss ts
    | null ts && null ss = 0
    | length finished > 0 = totalDuration w (filter (\c -> not $ elem (condDep c) finished) cs) ss (filter (not . isDone) ts)
    | length ts < w && isJust next = let next = nextToRun cs ss in
        totalDuration w cs (filter (/= (fromJust next)) ss) ((fromStep $ (fromJust next)) : ts)
    | otherwise = 1 + (totalDuration w cs ss ((\ts -> traceShow ts ts) $ map tick ts))
    where
        finished = map toStep $ filter isDone ts
        next = nextToRun cs ss

solve :: [Condition] -> Int
solve cs = totalDuration 5 cs (allSteps cs) []

main :: IO()
main = interact (show . solve . parse)

