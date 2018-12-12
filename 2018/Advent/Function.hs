module Advent.Function where

generate :: (a -> a) -> a -> [a]
generate f x = scanl (\y _ -> f y) x [0..]

windows :: Int -> [a] -> [[a]]
windows = windowsWith id

windowsWith :: ([a] -> b) -> Int -> [a] -> [b]
windowsWith f n xs | length xs >= n = f (take n xs) : windowsWith f n (tail xs)
                   | otherwise = []

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [l] = map (\x -> [x]) l
transpose (l:ls) = zipWith (:) l $ transpose ls

repeatedly :: Int -> (a -> a) -> a -> a
repeatedly 0 _ = id
repeatedly n f = repeatedly (n-1) f . f
