import Day8

nodeFold :: (a -> Node -> a) -> a -> Node -> a
nodeFold f x n | null $ children n = f x n
               | otherwise = f (foldl (nodeFold f) x $ children n) n

solve :: Tree -> Int
solve t = nodeFold (\x n -> foldl (+) x $ metadata n) 0 $ root t

main :: IO()
main = interact (show . solve . parse)
