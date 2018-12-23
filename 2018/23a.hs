import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

import Advent.Strings

import Debug.Trace

data Nanobot = Nanobot { nanoX :: Int, nanoY :: Int, nanoZ :: Int, nanoRange :: Int } deriving (Eq, Ord)

instance Show Nanobot where
    show (Nanobot x y z r) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show r ++ "]"

parseLine :: String -> Nanobot
parseLine s = Nanobot (read x) (read y) (read z) (read r) where
    (x:y:z:r:_) = words $ replace "pos=<>,r" ' ' s

parse :: String -> [Nanobot]
parse = map parseLine . lines

location :: Nanobot -> (Int, Int, Int)
location (Nanobot x y z _) = (x, y, z)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distance (x1, y1, z1) (x2, y2, z2) = dx + dy + dz where
    dx = abs $ x1 - x2
    dy = abs $ y1 - y2
    dz = abs $ z1 - z2

inRange :: Nanobot -> (Int, Int, Int) -> Bool
inRange (Nanobot x y z r) c = r >= distance (x, y, z) c

solveA :: [Nanobot] -> Int
solveA bots = length $ filter (inRange b) $ map location $ bots where
    b = head $ sortBy (compare `on` (negate . nanoRange)) bots

data Graph a = Graph { edges :: Map.Map a (Set.Set a) }

overlap :: Nanobot -> Nanobot -> Bool
overlap n1 n2 = distance (location n1) (location n2) <= (nanoRange n1) + (nanoRange n2)

constructGraph :: [Nanobot] -> Graph Nanobot
constructGraph ns = Graph $ Map.fromList $ map (\n -> (n, Set.fromList $ filter (\n' -> overlap n n' && n /= n') ns)) ns

canExtend :: Ord a => Graph a -> Set.Set a -> a -> Bool
canExtend (Graph edges) sg n = (not alreadyIn) && isConnected where
    alreadyIn = Set.member n sg
    isConnected = all (\n' -> elem n' es) sg
    es = fromJust $ Map.lookup n edges

bronKerbosch :: Ord a => Show a => Graph a -> [Set.Set a]
bronKerbosch g = go Set.empty (Set.fromList $ map fst $ Map.toList $ edges g) Set.empty where
    go r p x | Set.null p && Set.null x = [r]
             | otherwise = innerLoop r (Set.toList p) x where
        innerLoop r' p'@(v:ps) x' = go (traceShow (length r' + 1) $ Set.insert v r') (Set.intersection (Set.fromList p') ns) (Set.intersection x' ns) ++ innerLoop r' ps (Set.insert v x') where
            ns = fromJust $ Map.lookup v $ edges g
        innerLoop r' [] x' = []

bounds :: Set.Set Nanobot -> ((Int, Int, Int), (Int, Int, Int))
bounds bots = ((minX, minY, minZ), (maxX, maxY, maxZ)) where
    minX = maximum $ map (\n -> nanoX n - nanoRange n) ns
    minY = maximum $ map (\n -> nanoY n - nanoRange n) ns
    minZ = maximum $ map (\n -> nanoZ n - nanoRange n) ns
    maxX = minimum $ map (\n -> nanoX n + nanoRange n) ns
    maxY = minimum $ map (\n -> nanoY n + nanoRange n) ns
    maxZ = minimum $ map (\n -> nanoZ n + nanoRange n) ns
    ns = Set.toList bots

solveB bots = overlap where
    overlap = filter (\c -> all (flip inRange c) maximal) candidates where
    candidates = [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
    ((x1,y1,z1), (x2,y2,z2)) = bounds maximal
    maximal = head $ sortBy (compare `on` (negate . length)) sgs
    sgs = bronKerbosch $ constructGraph bots

solve bots = (solveA bots)

main :: IO()
main = interact (show . solveB . parse)
