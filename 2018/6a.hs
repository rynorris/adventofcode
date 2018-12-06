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

expand :: Set.Set Coord -> Set.Set Coord
expand cs = Set.fold (\c s -> Set.union s (neighbours c)) Set.empty cs

overlaps :: [Set.Set Coord] -> [Coord]
overlaps = map head . filter (not . null . tail) . groupBy (==) . sort . foldl1 (++) . map Set.elems

removeOverlaps :: [Set.Set Coord] -> [Set.Set Coord]
removeOverlaps territories = map (Set.filter (\c -> not $ elem c os)) territories where
    os = overlaps territories

outOfBounds :: Int -> Int -> Int -> Int -> Coord -> Bool
outOfBounds x1 x2 y1 y2 (Coord x y) = x < x1 || x > x2 || y < y1 || y > y2

limitBounds :: Int -> Int -> Int -> Int -> Set.Set Coord -> Set.Set Coord
limitBounds x1 x2 y1 y2 = Set.filter (not . outOfBounds x1 x2 y1 y2)

step :: Int -> Int -> Int -> Int -> [Set.Set Coord] -> [Set.Set Coord]
step x1 x2 y1 y2 territories = map (uncurry Set.union) $ zip territories expansions where
    expansions = map (limitBounds x1 x2 y1 y2) $ removeOverlaps $ map expand territories
    

findSteadyState :: Int -> Int -> Int -> Int -> [Set.Set Coord] -> [Set.Set Coord]
findSteadyState x1 x2 y1 y2 territories | next == territories = territories
                                        | otherwise = findSteadyState x1 x2 y1 y2 next where
    next = step x1 x2 y1 y2 territories

touchesEdge :: Int -> Int -> Int -> Int -> Coord -> Bool
touchesEdge x1 x2 y1 y2 (Coord x y) = x == x1 || x == x2 || y == y1 || y == y2

isInfiniteSize :: Int -> Int -> Int -> Int -> Set.Set Coord -> Bool
isInfiniteSize x1 x2 y1 y2 = not . null . filter (touchesEdge x1 x2 y1 y2) . Set.elems

solve :: [Coord] -> Int
solve cs = foldl1 max $ map Set.size $ filter (not . (isInfiniteSize x1 x2 y1 y2)) $ findSteadyState x1 x2 y1 y2 $ map Set.singleton cs where
    x1 = foldl1 min $ map coordX cs
    x2 = foldl1 max $ map coordX cs
    y1 = foldl1 min $ map coordY cs
    y2 = foldl1 max $ map coordY cs

main :: IO()
main = interact (show . solve . parse)
