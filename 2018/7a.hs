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

nextToRun :: [Condition] -> [Step] -> Step
nextToRun cs ss = head $ filter (canRun cs) ss

order :: [Condition] -> [Step] -> [Step]
order cs ss | null cs = ss
            | otherwise = let next = nextToRun cs ss in
            next : order (filter (\c -> condDep c /= next) cs) (filter (/= next) ss)

solve :: [Condition] -> [Step]
solve cs = order cs (allSteps cs)

main :: IO()
main = interact (show . solve . parse)
