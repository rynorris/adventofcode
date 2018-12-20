import Advent.Plane

import Debug.Trace

data Direction = North | East | South | West deriving (Show, Eq, Ord)
type Route = [Element]
data Element = Choices [Element] | Path [Element] | Step Direction deriving (Show, Eq, Ord)

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
    go s es = let (e, s') = consumePath s in go (traceShow s' s') (e : es)

parse :: String -> Route
parse s = go (filter (\c -> elem c "NESW()|^$") s) [] where
    go "" r = reverse r
    go s r = let (e, s') = consumeElement s in go s' (e:r)

solve :: Route -> Int
solve r = 0

debug = id

main :: IO()
main = interact (show . debug . parse)
