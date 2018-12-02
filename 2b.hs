differingLetters :: String -> String -> [(Char, Char)]
differingLetters s t = filter (not . (uncurry (==))) $ zip s t

isCloseMatch :: String -> String -> Bool
isCloseMatch s t = (== 1) $ length $ differingLetters s t

solve :: [String] -> (String, String)
solve xs = head $ filter (uncurry isCloseMatch) [(x, y) | x <- xs, y <- xs]

main :: IO()
main = interact (show . solve . words)
