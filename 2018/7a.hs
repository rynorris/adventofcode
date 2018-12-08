import Day7

import Data.List 
import Data.Maybe

order :: [Condition] -> [Step] -> [Step]
order cs ss | null cs = ss
            | isJust next = let next = nextToRun cs ss in
            fromJust next : order (filter (\c -> dep c /= fromJust next) cs) (filter (/= (fromJust next)) ss)
    where next = nextToRun cs ss

solve :: [Condition] -> [Step]
solve cs = order cs (allSteps cs)

main :: IO()
main = interact (show . solve . parse)
