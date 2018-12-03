parse = map read . map (dropWhile (=='+')) . words

append xs x = xs ++ [x]

firstDupe xs = fst $ head $ filter (uncurry elem) $ zip xs (scanl append [0] xs)

solve = firstDupe . scanl1 (+) . cycle

main = interact (show . solve . parse)

