import Day3

import Data.List

innerCoords :: [Int] -> [(Int, Int)]
innerCoords square = [(x, y) |
    x <- [square !! 0 .. (square !! 0 + square !! 2 - 1)],
    y <- [square !! 1 .. (square !! 1 + square !! 3 - 1)]]

listDupes :: [[Int]] -> [(Int, Int)]
listDupes squares = map (!! 1) $ filter (\l -> length l >= 2) $ groupBy (==) $ sort $ foldl (++) [] $ map innerCoords squares

hasDupes :: [(Int, Int)] -> [Int] -> Bool
hasDupes dupes square = (> 0) $ length $ filter (contains square) dupes

solve :: [[Int]] -> Int
solve squares = length $ takeWhile id $ map (hasDupes dupes) squares where
    dupes = listDupes squares

main :: IO()
main = interact (show . solve . parse)

