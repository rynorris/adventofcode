import Data.List

import Advent.IndexedList;

data Kitchen = Kitchen { recipes :: IndexedList Int, elf1 :: Int, elf2 :: Int } deriving (Show)

digits :: Int -> [Int]
digits n = if null ds then [0] else ds where
    ds = reverse $ go n
    go 0 = []
    go n = (mod n 10) : go (div n 10)

combine :: Int -> Int -> [Int]
combine x y = digits (x+y)

nextRecipes :: Kitchen -> [Int]
nextRecipes k = combine r1 r2 where
    r1 = getIndex (recipes k) (elf1 k)
    r2 = getIndex (recipes k) (elf2 k)

addRecipes :: Kitchen -> [Int] -> Kitchen
addRecipes (Kitchen rs e1 e2) newRs = Kitchen rs' e1' e2' where
    rs' = foldl append rs newRs
    e1' = flip mod (listLength rs') $ e1 + (getIndex rs e1) + 1
    e2' = flip mod (listLength rs') $ e2 + (getIndex rs e2) + 1

allRecipes :: Kitchen -> [Int]
allRecipes k = rs ++ (allRecipes k') where
    rs = nextRecipes k
    k' = addRecipes k rs

initialKitchen :: Kitchen
initialKitchen = Kitchen firstRecipes 0 1 where
    firstRecipes = foldl (\l x -> append l x) emptyList [3, 7]

solveA :: Int -> String
solveA n = concat $ map show $ take 10 $ drop (n-2) $ allRecipes $ initialKitchen

findSeq :: [Int] -> [Int] -> Int
findSeq l xs = length $ takeWhile (not . isPrefixOf xs) $ tails l

solveB :: [Int] -> Int
solveB xs = (+ 2) $ flip findSeq xs $ allRecipes $ initialKitchen

main :: IO()
main = interact (show . solveB . read)

