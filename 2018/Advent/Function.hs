module Advent.Function where

generate :: (a -> a) -> a -> [a]
generate f x = scanl (\y _ -> f y) x [0..]
