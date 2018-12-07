module Day7 where

import Data.List

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

