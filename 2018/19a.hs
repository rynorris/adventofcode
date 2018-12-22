import ElfScript

import Advent.IndexedList

parse = parseCPU

timeline :: CPU -> [CPU]
timeline = iterate step

solve :: CPU -> [Int]
solve = registers . head . snd . break shouldBreak . timeline

simpleProg :: Int -> Int
simpleProg n = foldl (+) 0 $ map (\x -> if mod n x == 0 then div n x else 0) $ [1..n]

solveA :: Int
solveA = simpleProg 967

solveB :: Int
solveB = simpleProg 10551367

debug :: CPU -> String
debug cpu@(CPU rs prog po pr) = unlines $ map show $ take 100 $ map (\c -> (pointer c, registers c)) $ timeline cpu' where
    cpu' = CPU (setRegister 0 1 rs) prog po pr

main :: IO()
main = do putStr $ show $ (solveA, solveB)
