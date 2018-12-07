import Day7

import Data.List 

order :: [Condition] -> [Step] -> [Step]
order cs ss | null cs = ss
            | otherwise = let next = nextToRun cs ss in
            next : order (filter (\c -> condDep c /= next) cs) (filter (/= next) ss)

solve :: [Condition] -> [Step]
solve cs = order cs (allSteps cs)

main :: IO()
main = interact (show . solve . parse)
