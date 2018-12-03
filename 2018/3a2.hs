import Data.List

replace :: String -> Char -> String -> String
replace chars target s = map (\c -> if (elem c chars) then target else c) s

parseLine :: String -> [Int]
parseLine s = map read $ drop 1 $ words $ replace "#@,:x" ' ' s

innerCoords :: [Int] -> [(Int, Int)]
innerCoords square = [(x, y) |
    x <- [square !! 0 .. (square !! 0 + square !! 2 - 1)],
    y <- [square !! 1 .. (square !! 1 + square !! 3 - 1)]]

listDupes :: [[Int]] -> [(Int, Int)]
listDupes squares = map (!! 1) $ filter (\l -> length l >= 2) $ groupBy (==) $ sort $ foldl (++) [] $ map innerCoords squares

solve :: [[Int]] -> Int
solve = length . listDupes

main :: IO()
main = interact (show . solve . map parseLine . lines)


