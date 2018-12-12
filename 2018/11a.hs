import Advent.Plane

serialNumber = 7857

rackId :: Coord -> Int
rackId = (+ 10) . coordX

hundredsDigit :: Int -> Int
hundredsDigit = flip mod 10 . flip div 100

powerLevel :: Coord -> Int
powerLevel c@(Coord x y) = (flip (-) 5) $ hundredsDigit $ (* r) $ (+ serialNumber) $ r * y where
    r = rackId c

grid :: Int -> Int -> Int -> Int -> [[Coord]]
grid x1 y1 x2 y2 = [[Coord x y | x <- [x1..x2]] | y <- [y1..y2]]

windows :: Int -> [a] -> [[a]]
windows n xs | length xs >= n = take n xs : windows n (tail xs)
             | otherwise = []

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [l] = map (\x -> [x]) l
transpose (l:ls) = zipWith (:) l $ transpose ls

areaPowers :: Int -> [[Coord]] -> [[Int]]
areaPowers size = transpose . map sumAreas . transpose . map sumAreas . map (map powerLevel)
    where sumAreas = map (foldl (+) 0) . windows size

foldIx :: (Int -> b -> a -> b) -> b -> [a] -> b
foldIx f v xs = go f 0 v xs where
    go f ix v [] = v
    go f ix v (x:xs) = go f (ix+1) (f ix v x) xs

maxIx :: Int -> (Int, Int) -> Int -> (Int, Int)
maxIx ix (maxIx, max) x | x > max = (ix, x)
                        | otherwise = (maxIx, max)

maxCoord :: Int -> (Coord, Int) -> (Int, Int) -> (Coord, Int)
maxCoord y (maxC, max) (x, v) | v > max = (Coord x y, v)
                              | otherwise = (maxC, max)

maxBestSquare :: Int -> (Coord, Int, Int) -> (Coord, Int) -> (Coord, Int, Int)
maxBestSquare size (maxC, max, maxSize) (c, v) | v > max = (c, v, size)
                                               | otherwise = (maxC, max, maxSize)

bestSquare :: Int -> (Coord, Int)
bestSquare size = collapseY $ map collapseX $ areaPowers size $ grid 1 1 300 300 where
    collapseX xs = foldIx maxIx (0, head xs) (tail xs)
    collapseY ys = foldIx maxCoord (Coord (fst $ head ys) 0, snd $ head ys) (tail ys)

bestestSquare :: (Coord, Int, Int)
bestestSquare = foldIx maxBestSquare (fst $ head bestSquares, snd $ head bestSquares, 0) (tail bestSquares) where
    bestSquares =  map bestSquare [1..300]
