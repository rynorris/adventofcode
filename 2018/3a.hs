import Day3

countContainers :: [[Int]] -> Int -> Int -> Int
countContainers squares x y = length $ take 2 $ filter (\s -> contains s (x, y)) squares

countOverlaps :: Int -> Int -> [[Int]] -> Int
countOverlaps w h squares = length $ filter (>= 2) $ map (uncurry (countContainers squares)) cells where
    cells = [(x, y) | x <- [0..w], y <- [0..h]]

solve :: [[Int]] -> Int
solve squares = countOverlaps w h squares where
    w = foldl1 max $ map (\s -> (s !! 0) + (s !! 2)) squares
    h = foldl1 max $ map (\s -> (s !! 1) + (s !! 3)) squares

main :: IO()
main = interact (show . solve . map parseLine . lines)
