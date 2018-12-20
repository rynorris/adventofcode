module Advent.Plane where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Plane a = Plane { objects :: Map.Map Coord a } deriving (Show, Eq, Ord)

emptyPlane :: Plane a
emptyPlane = Plane Map.empty

addObject :: Coord -> a -> Plane a -> Plane a
addObject c o (Plane os) = Plane (Map.insert c o os)

addObjectIfEmpty :: Coord -> a -> Plane a -> Plane a
addObjectIfEmpty c o p = if (isNothing $ getObject c p) then addObject c o p else p

removeObject :: Coord -> Plane a -> Plane a
removeObject c (Plane os) = Plane (Map.delete c os)

getObject :: Coord -> Plane a -> Maybe a
getObject c (Plane os) = Map.lookup c os

getObjectOr :: Coord -> a -> Plane a -> a
getObjectOr c x p = maybe x id $ getObject c p

fromList :: [(Coord, a)] -> Plane a
fromList = Plane . Map.fromList

toList :: Plane a -> [(Coord, a)]
toList = Map.toList . objects

filterPlane :: (Coord -> a -> Bool) -> Plane a -> Plane a
filterPlane pred p = Plane (Map.filterWithKey pred $ objects p)

coordX :: Coord -> Int
coordX (Coord x _) = x

coordY :: Coord -> Int
coordY (Coord _ y) = y

coordAdd :: Coord -> Coord -> Coord
coordAdd (Coord x y) (Coord u v) = Coord (x+u) (y+v)

readingOrder :: Coord -> Coord -> Ordering
readingOrder (Coord x1 y1) (Coord x2 y2) | y1 == y2 = compare x1 x2
                                         | otherwise = compare y1 y2

above :: Coord -> Coord
above (Coord x y) = Coord x (y-1)

below :: Coord -> Coord
below (Coord x y) = Coord x (y+1)

left :: Coord -> Coord
left (Coord x y) = Coord (x-1) y

right :: Coord -> Coord
right (Coord x y) = Coord (x+1) y

neighbours :: Coord -> [Coord]
neighbours c = [above c, below c, left c, right c]

diags :: Coord -> [Coord]
diags c = [(above . left) c, (above . right) c, (below . left) c, (below . right) c]

drawCoords :: [Coord] -> String
drawCoords cs = unlines $ map (drawLineObjects (\x -> '#') minX) $ splitLines $ map (\c -> (c, True)) cs where
          minX = foldl1 min $ map (\(Coord x y) -> x) cs

splitLines :: Ord a => [(Coord, a)] -> [[(Coord, a)]]
splitLines cs = map sort $ map (\y -> filter (\(Coord _ y1, o) -> y1 == y) cs) $ [minY..maxY]
    where minY = foldl1 min $ map (coordY . fst) cs
          maxY = foldl1 max $ map (coordY . fst) cs

drawLineObjects :: Ord a => (a -> Char) -> Int -> [(Coord, a)] -> String
drawLineObjects _ _ [] = ""
drawLineObjects f x (c@(Coord x1 _, o):cs) | x == x1 = f o : drawLineObjects f (x+1) cs
                                           | x > x1 = drawLineObjects f x cs
                                           | otherwise = '.' : drawLineObjects f (x+1) (c:cs)

drawObjects :: Ord a => (a -> Char) -> [(Coord, a)] -> String
drawObjects f os = unlines $ map (drawLineObjects f minX) $ splitLines os where
          minX = foldl1 min $ map (coordX . fst) os

drawPlane :: Ord a => (a -> Char) -> Plane a -> String
drawPlane f p = drawObjects f $ toList p

parsePlane :: (Char -> a) -> String -> Plane a
parsePlane f s = fromList $ concat $  map parseLine $ zip [0..] $ lines s where
    parseLine (y,l) = map (\(x,c) -> (Coord x y, f c)) $ zip [0..] l
