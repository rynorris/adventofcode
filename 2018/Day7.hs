module Day7 where

import Data.List

type Step = Char
data Condition = Condition { dep :: Step, step :: Step } deriving (Show, Eq, Ord)

parseCondition :: String -> Condition
parseCondition line = Condition (head (ws !! 1)) (head (ws !! 7)) where
    ws = words line

parse :: String -> [Condition]
parse = map parseCondition . lines

allSteps :: [Condition] -> [Step]
allSteps cs = nub $ sort $ (map dep cs) ++ (map step cs)

canRun :: [Condition] -> Step -> Bool
canRun cs s = null $ filter (== s) $ map step cs

nextToRun :: [Condition] -> [Step] -> Maybe Step
nextToRun cs ss | null runnable  = Nothing
                | otherwise = Just (head runnable)
    where runnable = filter (canRun cs) ss

