import Day6

import Data.List
import qualified Data.Set as Set

setMinus :: (Ord a) => Set.Set a -> Set.Set a -> Set.Set a
setMinus s t = Set.filter (\x -> not $ Set.member x t) s

overlaps :: [Set.Set Coord] -> [Coord]
overlaps = map head . filter (not . null . tail) . groupBy (==) . sort . foldl1 (++) . map Set.elems

removeOverlaps :: [Set.Set Coord] -> [Set.Set Coord]
removeOverlaps cs = map (Set.filter (\c -> not $ elem c os)) cs where
    os = overlaps cs

step :: [Set.Set Coord] -> [Set.Set Coord] -> Set.Set Coord -> ([Set.Set Coord], [Set.Set Coord], Set.Set Coord)
step territories boundaries visited = (newTerritories, newBoundaries, newVisited) where
    newBoundaries = map (\b -> setMinus b newVisited) expandedBoundaries
    newVisited = Set.union (foldl1 Set.union boundaries) $ Set.union visited $ Set.fromList $ overlaps expandedBoundaries
    expandedBoundaries = map (\(t, b) -> setMinus b t) $ zip territories $ map boundary boundaries
    newTerritories = map (uncurry Set.union) $ zip territories boundaries
    
grow :: Int -> [Coord] -> ([Set.Set Coord], [Set.Set Coord], Set.Set Coord)
grow n cs = runFor n (\(a,b,c) -> step a b c) $ step (map Set.singleton cs) (map (boundary . Set.singleton) cs) Set.empty

solve :: [Coord] -> Int
solve seeds = foldl1 max $ map length $ map fst $ filter (\(a,b) -> length b == 0) $ (\(a,b,c) -> zip a b) $ grow 110 seeds

main :: IO()
main = interact (show . solve . parse)
