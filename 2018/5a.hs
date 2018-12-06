import Day5

solve :: [Unit] -> Int
solve = length . simplest

main :: IO()
main = interact (show . solve . parse)
