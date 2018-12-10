module Advent.Plane where

import Data.List

data Coord = Coord Int Int deriving (Show, Eq, Ord)

coordX :: Coord -> Int
coordX (Coord x _) = x

coordY :: Coord -> Int
coordY (Coord _ y) = y

coordAdd :: Coord -> Coord -> Coord
coordAdd (Coord x y) (Coord u v) = Coord (x+u) (y+v)

drawLine :: Int -> [Coord] -> String
drawLine _ [] = ""
drawLine x (c@(Coord x1 _):cs) | x == x1 = '*' : drawLine (x+1) cs
                               | x > x1 = drawLine x cs
                               | otherwise = ' ' : drawLine (x+1) (c:cs)

splitLines :: [Coord] -> [[Coord]]
splitLines cs = map sort $ map (\y -> filter (\(Coord _ y1) -> y1 == y) cs) $ [minY..maxY]
    where minY = foldl1 min $ map (\(Coord x y) -> y) cs
          maxY = foldl1 max $ map (\(Coord x y) -> y) cs

drawCoords :: [Coord] -> String
drawCoords cs = unlines $ map (drawLine minX) $ splitLines cs where
          minX = foldl1 min $ map (\(Coord x y) -> x) cs
