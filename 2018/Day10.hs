module Day10 where

import Advent.Function
import Advent.Plane

data Light = Light Coord Coord deriving (Show, Eq, Ord)

position :: Light -> Coord
position (Light x v) = x

parseLight :: String -> Light
parseLight s = Light (Coord x y) (Coord v w)
    where (x:y:v:w:rest) = map read $ words $ filter (flip elem "01213456789- ") s

parse :: String -> [Light]
parse = map parseLight . lines

step :: Light -> Light
step (Light x v) = Light (coordAdd x v) v

score :: [Light] -> Int
score ls = (maxX - minX) + (maxY - minY) where
    minX = minimum $ map coordX cs
    minY = minimum $ map coordY cs
    maxX = maximum $ map coordX cs
    maxY = maximum $ map coordY cs
    cs = map position ls

timeline :: [Light] -> [[Light]]
timeline = generate (map step)

