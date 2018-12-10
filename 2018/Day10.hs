module Day10 where

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data Light = Light Coord Coord deriving (Show, Eq, Ord)

position :: Light -> Coord
position (Light x v) = x

magnitude :: Coord -> Int
magnitude (Coord x y) = x + y

parseLight :: String -> Light
parseLight s = Light (Coord x y) (Coord v w)
    where (x:y:v:w:rest) = map read $ words $ filter (flip elem "01213456789- ") s

parse :: String -> [Light]
parse = map parseLight . lines

step :: Light -> Light
step (Light (Coord x y) (Coord u v)) = Light (Coord (x+u) (y+v)) (Coord u v)

score :: [Light] -> Int
score ls = (maxX - minX) + (maxY - minY) where
    minX = foldl1 min $ map (\(Coord x y)  -> x) cs
    minY = foldl1 min $ map (\(Coord x y)  -> y) cs
    maxX = foldl1 max $ map (\(Coord x y)  -> x) cs
    maxY = foldl1 max $ map (\(Coord x y)  -> y) cs
    cs = map position ls

timeline :: [Light] -> [[Light]]
timeline start = scanl (\ls _ -> map step ls) start [0..]

