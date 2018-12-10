import Day10

import Data.List

findMessage :: [[Light]] -> [Light]
findMessage (prev:cur:rest) | score cur > score prev = prev
                            | otherwise = findMessage (cur:rest)

drawLine :: Int -> [Coord] -> String
drawLine _ [] = ""
drawLine x (c@(Coord x1 _):cs) | x == x1 = '*' : drawLine (x+1) cs
                               | x > x1 = drawLine x cs
                               | otherwise = ' ' : drawLine (x+1) (c:cs)

splitLines :: [Coord] -> [[Coord]]
splitLines cs = map sort $ map (\y -> filter (\(Coord _ y1) -> y1 == y) cs) $ [minY..maxY]
    where minY = foldl1 min $ map (\(Coord x y) -> y) cs
          maxY = foldl1 max $ map (\(Coord x y) -> y) cs

draw :: [Coord] -> String
draw cs = unlines $ map (drawLine minX) $ splitLines cs where
          minX = foldl1 min $ map (\(Coord x y) -> x) cs

solve :: [Light] -> String
solve = draw . map position . findMessage . timeline

main :: IO()
main = interact (solve . parse)
