import Advent.Plane

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

import Debug.Trace

depth = 11541
target = Coord 14 778

data Terrain = Rocky | Wet | Narrow deriving (Show, Eq, Ord)

drawTerrain :: Terrain -> Char
drawTerrain Rocky = '.'
drawTerrain Wet = '='
drawTerrain Narrow = '|'

erosionMap :: Int -> Coord -> Int -> Int -> Plane Int
erosionMap d (Coord tx ty) mx my = go emptyPlane (Coord 0 0) where
    go p c@(Coord 0 0) = go (addObject c (erosion 0) p) (next c)
    go p c@(Coord 0 y) = go (addObject c (erosion $ y * 48271) p) (next c)
    go p c@(Coord x 0) = go (addObject c (erosion $ x * 16807) p) (next c)
    go p c@(Coord x y) | y > my = p
                       | x == tx && y == ty = go (addObject c (erosion 0) p) (next c)
                       | otherwise = go (addObject c (erosion $ (getObjectOr (left c) 0 p) * (getObjectOr (above c) 0 p)) p) (next c)
    next c@(Coord x y) = if x == mx then Coord 0 (y+1) else right c
    erosion x = (x + d) `mod` 20183

terrain :: Int -> Terrain
terrain x | x `mod` 3 == 0 = Rocky
          | x `mod` 3 == 1 = Wet
          | x `mod` 3 == 2 = Narrow

terrainMap :: Plane Int -> Plane Terrain
terrainMap = mapPlane terrain

terrainRisk :: Terrain -> Int
terrainRisk Rocky = 0
terrainRisk Wet = 1
terrainRisk Narrow = 2

riskLevel :: Plane Terrain -> Int
riskLevel p = foldl1 (+) $ map (terrainRisk . snd) $ toList p

solveA :: Int
solveA = riskLevel $ terrainMap $ erosionMap depth target 100 1000

-- For part B we'll consider the problem as a 3D space with coordinates (x, y, Gear)
-- Then it's the problem of finding the shortest path from (0, 0, Neither) to (tx, ty, Torch)
-- We'll do so by considering all possible moves, keeping track of the shortest path to each square, and culling paths which aren't the shortest.
data Gear = Climbing | Torch | Neither deriving (Show, Eq, Ord)

type Position = (Coord, Gear)

type Route = [Position]

type DistanceMap = Map.Map Position (Int, Route)

isInBounds :: Coord -> Bool
isInBounds (Coord x y) = x >= 0 && y >= 0

canEquip :: Gear -> Terrain -> Bool
canEquip Neither Rocky = False
canEquip Torch Wet = False
canEquip Climbing Narrow = False
canEquip _ _ = True

possibleMoves :: Plane Terrain -> Position -> [(Int, Position)]
possibleMoves p (c, g) = coordMoves ++ gearMoves where
    coordMoves = map (\(t,c') -> (1, (c',g))) $ filter (canEquip g . fst) $ map (\(t,c') -> (fromJust t, c')) $ filter (isJust . fst) $ map (\c' -> (getObject c' p, c')) $ filter isInBounds $ neighbours c
    gearMoves = map (\g' -> (7, (c,g'))) $ filter (flip canEquip (fromJust $ getObject c p)) $ filter (/= g) $ [Climbing, Torch, Neither]

newRoutes :: Plane Terrain -> (Int, Route) -> Set.Set (Int, Route)
newRoutes p (s,r) = Set.fromList $ map (\(x,pos) -> ((x+s), pos:r)) $ possibleMoves p $ head r

updateDistanceMap :: Route -> Int -> DistanceMap -> DistanceMap
updateDistanceMap r s m = if s < s' || s' == -1 then Map.insert (head r) (s,r) m else m where
    (s',_) = maybe (-1, []) id $ Map.lookup (head r) m

cullRoute :: DistanceMap -> Position -> (Int, Route) -> Bool
cullRoute m t (s,r) = (s < s' || s' == -1) && (s < ts || ts == -1) where
    (s', _) = maybe (-1, []) id $ Map.lookup (head r) m
    (ts, _) = maybe (-1, []) id $ Map.lookup t m

canonicalize :: DistanceMap -> (Int, Route) -> (Int, Route)
canonicalize m (s,r) = maybe (s,r) id $ Map.lookup (head r) m

fastestRoute :: Plane Terrain -> Position -> Position -> (Int, Route)
fastestRoute tMap from to = go (Set.singleton (0, [from])) Map.empty where
    go routes dMap | Set.null routes = fromJust $ Map.lookup to dMap
                   | otherwise = let
        dMap' = foldl (\m (s,r) -> updateDistanceMap r s m) dMap $ Set.toList routes
        routes' = Set.filter (cullRoute dMap' to) $ Set.unions $ map (newRoutes tMap) $ Set.toList $ Set.map (canonicalize dMap') routes
        in
            go routes' dMap'

debugRoutes :: Set.Set (Int, Route) -> Set.Set (Int, Route)
debugRoutes rs = traceShow (n,mx,my,minC,maxC) rs where
    minC = head $ sortBy (compare `on` (\(Coord x y) -> x + y)) cs
    maxC = head $ sortBy (compare `on` (\(Coord x y) -> -(x + y))) cs
    my = maximum $ map (coordY . fst . head) lrs
    mx = maximum $ map (coordX . fst . head) lrs
    n = length lrs
    cs = map fst $ map head lrs
    lrs = map snd $ Set.toList rs

solveB :: (Int, Route)
solveB = fastestRoute tMap (Coord 0 0, Torch) (target, Torch) where
    tMap = terrainMap $ erosionMap depth target 100 1000

solve :: (Int, Int)
solve = (solveA, fst $ solveB)

main :: IO()
main = do putStr (show $ solve)
