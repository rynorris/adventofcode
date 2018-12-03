import Data.List

replace :: String -> Char -> String -> String
replace chars target s = map (\c -> if (elem c chars) then target else c) s

parseLine :: String -> [Int]
parseLine s = map read $ drop 1 $ words $ replace "#@,:x" ' ' s

contains :: [Int] -> (Int, Int) -> Bool
contains square (x, y) = foldl1 (&&) [
    (x >= square !! 0),
    (x < (square !! 0) + (square !! 2)),
    (y >= square !! 1),
    (y < (square !! 1) + (square !! 3))]

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
main = interact (show . solve . map parseLine . lines)

