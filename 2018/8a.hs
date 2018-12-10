import Day8

import Advent.Tree

solve :: Tree Metadata -> Int
solve t = nodeFold (\x n -> foldl (+) x $ metadata n) 0 $ root t

main :: IO()
main = interact (show . solve . parse)
