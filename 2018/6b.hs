import Day6

distance :: Coord -> Coord -> Int
distance (Coord x1 y1) (Coord x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

step :: Int -> [(Coord, Int)] -> Coord -> [(Coord, Int)]
step n cs x = filter (\(c, d) -> d < n) $ map (\(c, d) -> (c, d + (distance c x))) cs

solve :: [Coord] -> Int
solve cs = length $ foldl (step 10000) universe cs where
    universe = [(Coord x y, 0) | x <- [0..500], y <- [0..500]]

main :: IO()
main = interact (show . solve . parse)

