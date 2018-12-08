import Day8

value :: Node -> Int
value n | null $ children n = foldl (+) 0 $ metadata n
        | otherwise = foldl (+) 0 $ map value $ map ((!!) $ children n) $ filter (< (length $ children n)) $ map (\x -> x-1) $ metadata n

solve = value . root

main :: IO()
main = interact (show . solve . parse)

