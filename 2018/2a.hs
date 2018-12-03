import Data.List

letterCounts = map length . groupBy (==) . sort

hasRepeat :: Int -> String -> Bool
hasRepeat n s = not $ null $ filter (== n) $ letterCounts s

checksum :: [String] -> Int
checksum xs = n * m where
    n = length $ filter (hasRepeat 2) $ xs
    m = length $ filter (hasRepeat 3) $ xs

main :: IO()
main = interact (show . checksum . words)
