module Day3 where

import Common

parseLine :: String -> [Int]
parseLine s = map read $ drop 1 $ words $ replace "#@,:x" ' ' s

parse :: String -> [[Int]]
parse = map parseLine . lines

contains :: [Int] -> (Int, Int) -> Bool
contains square (x, y) = foldl1 (&&) [
    (x >= square !! 0),
    (x < (square !! 0) + (square !! 2)),
    (y >= square !! 1),
    (y < (square !! 1) + (square !! 3))]

