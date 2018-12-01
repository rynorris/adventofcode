solve input = show $ sum $ map read $ map (dropWhile (== '+')) $ words input

main = interact solve
