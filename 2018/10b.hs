import Day10

timeUntilMessage :: [[Light]] -> Int
timeUntilMessage (prev:cur:rest) | score cur > score prev = 0
                                 | otherwise = 1 + timeUntilMessage (cur:rest)

solve :: [Light] -> Int
solve = timeUntilMessage . timeline

main :: IO()
main = interact (show . solve . parse)
