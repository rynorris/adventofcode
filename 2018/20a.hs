import Advent.Plane

import qualified Data.Set as Set

import Debug.Trace

data Direction = North | East | South | West deriving (Show, Eq, Ord)
type Route = [Element]
data Element = Choices [Element] | Path [Element] | Step Direction deriving (Show, Eq, Ord)

data Object = Start | Wall | Door | Room deriving (Show, Eq, Ord)
type Map = Plane Object

drawObject :: Object -> Char
drawObject Wall = '#'
drawObject Door = '+'
drawObject Room = '.'
drawObject Start = 'X'

-- Parsing

parseDirection :: Char -> Direction
parseDirection 'N' = North
parseDirection 'E' = East
parseDirection 'S' = South
parseDirection 'W' = West

consumePath :: String -> (Element, String)
consumePath s = go s [] where
    go t@(c:cs) p | elem c "NESW" = go cs (Step (parseDirection c) : p)
                  | c == '(' = let (e, cs') = consumeElement cs in go cs' (e : p)
                  | otherwise = (Path (reverse p), t)

consumeElement :: String -> (Element, String)
consumeElement s = go s [] where
    go ('^':cs) es = go cs es
    go ('(':cs) es = let (e, cs') = consumeElement cs in go cs' (e : es)
    go ('|':cs) es = let (p, cs') = consumePath cs in go cs' (p : es)
    go (')':cs) es = (Choices (reverse es), cs)
    go ('$':cs) es = (head es, cs)
    go s es = let (e, s') = consumePath s in go s' (e : es)

parse :: String -> Route
parse s = go (filter (\c -> elem c "NESW()|^$") s) [] where
    go "" r = reverse r
    go s r = let (e, s') = consumeElement s in go s' (e:r)

-- Computing

move :: Direction -> Coord -> Coord
move North c = above c
move East c = right c
move South c = below c
move West c = left c

constructMap :: Route -> Map
constructMap r = go r (fromList [((Coord 0 0), Start)]) (Coord 0 0) where
    go [] m c = m
    go (Step d:r') m c = let mv = move d in go r' (addObject (mv $ mv c) Room $ addObject (mv c) Door m) (mv $ mv c)
    go (Path es:r') m c = go r' (go es m c) c
    go (Choices cs:r') m c = go r' (foldl (\m e -> go [e] m c) m cs) c

fillMap :: Map -> Map
fillMap m = foldl (\m c -> addObjectIfEmpty c Wall m) m $ [Coord x y | x <- [minX..maxX], y <- [minY..maxY]] where
    minX = (\x -> x - 1) $ minimum $ map coordX cs
    maxX = (\x -> x + 1) $ maximum $ map coordX cs
    minY = (\x -> x - 1) $ minimum $ map coordY cs
    maxY = (\x -> x + 1) $ maximum $ map coordY cs
    cs = map fst $ toList m

distanceMap :: Map -> Plane Int
distanceMap m = go m emptyPlane (Set.singleton $ fst $ head $ filter ((== Start) . snd) $ toList m) 0 where
    go m dm wave d | Set.null wave = dm
                   | otherwise = go m dm' wave' (d+1) where
        dm' = foldl (\p c -> addObjectIfEmpty c d p) dm wave
        wave' = Set.fromList $ filter (\c -> (/= Wall) $ getObjectOr c Wall m) $ filter (flip isEmpty dm) $ concat $ map neighbours $ Set.toList wave

solve :: Route -> (Int, Int)
solve r = (partA, partB) where
    partA = (\x -> x `div` 2) $ maximum ds
    partB = length $ filter (\d -> d `div` 2 >= 1000) ds
    ds = map snd $ filter (\(c,d) -> (== Room) $ getObjectOr c Wall m) $ toList dm
    dm = distanceMap m
    m = fillMap $ constructMap r

debug = drawPlane (last . show) . distanceMap . fillMap . constructMap

main :: IO()
main = interact (show . solve . parse)
