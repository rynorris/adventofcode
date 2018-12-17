import Advent.Plane
import Advent.Strings

import Data.List
import Data.Maybe

import Debug.Trace

data Object = Clay | Flow | Still | Spring deriving (Show, Eq, Ord)

drawObject :: Object -> Char
drawObject Clay = '#'
drawObject Flow = '|'
drawObject Still = '~'
drawObject Spring = '*'

parseRange :: String -> (Int, Int)
parseRange s = (xs !! 0, xs !! 1) where 
    xs = map read $ words $ replace "." ' ' s

parseLine :: String -> [Coord]
parseLine s = zipWith combiner (repeat val) [rs..re] where
    (rs,re) = parseRange (parts !! 3)
    val = read (parts !! 1)
    combiner = if head parts == "x" then Coord else flip Coord
    parts = words $ replace ",=" ' ' s

parse :: String -> Plane Object
parse s = fromList $  map (\c -> (c, Clay)) $ concat $ map parseLine $ lines s

addSpring :: Plane Object -> Plane Object
addSpring p = addObject (Coord 500 0) Spring p

isPassable :: Object -> Bool
isPassable Clay = False
isPassable Still = False
isPassable _ = True

isFlowable :: Object -> Bool
isFlowable Flow = True
isFlowable Spring = True
isFlowable _ = False

isWater :: Object -> Bool
isWater Flow = True
isWater Spring = True
isWater Still = True
isWater _ = False

canSpreadTo :: Plane Object -> Coord -> Bool
canSpreadTo p c = maybe True isPassable (getObject c p)

spread :: Plane Object -> Coord -> [Coord]
spread p c = if canSpreadTo p b then [b] else filter (canSpreadTo p) s where
    b = below c
    s = [left c, right c]

flow :: Plane Object -> Plane Object
flow plane = foldl (\p c -> addObject c Flow p) plane $ concat $ map (spread plane) flows where
    flows = map fst $ filter (isFlowable . snd) $ toList plane

horizontalLines :: [Coord] -> [[Coord]]
horizontalLines cs = map reverse $ hLines (sortBy readingOrder cs) [] where

hLines :: [Coord] -> [[Coord]] -> [[Coord]]
hLines [] rs = rs
hLines (x:xs) [] = hLines xs [[x]]
hLines (x:xs) (r:rs) = if (coordX x) - (coordX $ head r) == 1 && coordY x == coordY (head r) then hLines xs ((x:r):rs) else hLines xs ([x]:r:rs)

canSettle :: Plane Object -> [Coord] -> Bool
canSettle p cs = solidBelow && constrainedSides where
    constrainedSides = null $ filter id $ map (maybe True (not . flip elem [Clay, Flow])) sides
    sides = map (flip getObject p) $ concat $ map (\c -> [left c, right c]) cs
    solidBelow = null $ filter (canSpreadTo p) $ map below cs

settle :: Plane Object -> Plane Object
settle plane = foldl (\p (c, o) -> addObject c o p) plane $ map (\c -> (c, Still)) $ concat $ filter (canSettle plane) ls where
    ls = horizontalLines flows
    flows = map fst $ filter (isFlowable . snd) $ toList plane

constrain :: Int -> Int -> Plane Object -> Plane Object
constrain minY maxY p = foldl (\pl (c,o) -> removeObject c pl) p $ filter (\(Coord x y, o) -> minY > y ||  y > maxY) $ toList p

timeline :: Plane Object -> [Plane Object]
timeline p = iterate (constrain 0 maxY . settle . flow) p where
    maxY = maximum $ map (coordY . fst) $ toList p

findSteadyState :: Plane Object -> Plane Object
findSteadyState p = fst $ head $ dropWhile (\(t1, t2) -> t1 /= t2) $ zip t (tail t) where
    t = timeline p

solveA :: Plane Object -> Int
solveA p = length $ filter (isWater . snd) $ toList $ constrain minY maxY $ findSteadyState $ addSpring p where
    minY = minimum $ map (coordY . fst) $ toList p
    maxY = maximum $ map (coordY . fst) $ toList p

solveB :: Plane Object -> Int
solveB p = length $ filter ((== Still) . snd) $ toList $ constrain minY maxY $ findSteadyState $ addSpring p where
    minY = minimum $ map (coordY . fst) $ toList p
    maxY = maximum $ map (coordY . fst) $ toList p

main :: IO()
main = interact (show . solveB . parse)
