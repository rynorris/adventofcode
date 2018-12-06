module Day6 where

import Data.List
import qualified Data.Set as Set

data Coord = Coord Int Int deriving (Show, Eq, Ord)

coordX :: Coord -> Int
coordX (Coord x _) = x

coordY :: Coord -> Int
coordY (Coord _ y) = y

replace :: String -> Char -> String -> String
replace chars target s = map (\c -> if (elem c chars) then target else c) s

splitCoords :: [Int] -> [Coord]
splitCoords (u:v:us) = Coord u v : splitCoords us
splitCoords [] = []

parse :: String -> [Coord]
parse = splitCoords . map read . words . (replace "," ' ')

neighbours :: Coord -> Set.Set Coord
neighbours (Coord u v) = Set.fromList [Coord (u-1) v, Coord (u+1) v, Coord u (v-1), Coord u (v+1)]

boundary :: Set.Set Coord -> Set.Set Coord
boundary cs = Set.fold (\c s -> Set.union s (neighbours c)) Set.empty cs

runFor :: Int -> (a -> a) -> a -> a
runFor 0 f x = x
runFor n f x = runFor (n-1) f (f x)
