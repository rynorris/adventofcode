module Day6 where

import Advent.Strings
import Advent.Plane

import Data.List
import qualified Data.Set as Set

splitCoords :: [Int] -> [Coord]
splitCoords (u:v:us) = Coord u v : splitCoords us
splitCoords [] = []

parse :: String -> [Coord]
parse = splitCoords . map read . words . (replace "," ' ')

runFor :: Int -> (a -> a) -> a -> a
runFor 0 f x = x
runFor n f x = runFor (n-1) f (f x)
