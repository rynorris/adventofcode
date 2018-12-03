parse = map read . map (dropWhile (=='+')) . words

main = interact (show . sum . parse)
