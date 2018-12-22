import ElfScript

import Debug.Trace

parse :: String -> CPU
parse = parseCPU

timeline :: CPU -> [CPU]
timeline = iterate step

runLength :: Int -> CPU -> Int
runLength n (CPU rs prog po pr) = length $ takeWhile (not . shouldBreak) $ timeline cpu' where
    cpu' = CPU (setRegister 0 n rs) prog po pr

solve :: CPU -> Int
solve cpu = runLength 16311888 cpu

debug (CPU rs prog po pr) = unlines $ map show $ map (\c -> (pointer c, registers c)) $ take 2000 $ timeline cpu' where
    cpu' = CPU (setRegister 0 16311888 rs) prog po pr

main :: IO()
main = interact (show . solve . parse)
