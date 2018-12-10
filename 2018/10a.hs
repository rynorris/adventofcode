import Day10

import Advent.Plane

import Data.List

findMessage :: [[Light]] -> [Light]
findMessage (prev:cur:rest) | score cur > score prev = prev
                            | otherwise = findMessage (cur:rest)

solve :: [Light] -> String
solve = drawCoords . map position . findMessage . timeline

main :: IO()
main = interact (solve . parse)
