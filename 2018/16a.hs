import ElfScript

parseExample :: [String] -> Example
parseExample (b:o:a:ls) = Example b' o' a' where
    b' = read $ drop (length "Before: ") b
    o' = map read $ words o
    a' = read $ drop (length "After: ") a

parse :: String -> ([Example], [[Int]])
parse s = go (lines s) [] where
    go (x:y:z:b:ls) es | null x && null y = (es, map (map read . words) ls)
                       | otherwise = go ls (parseExample (x:y:z:[]) : es)

-- Define instructions
instructions :: [Instruction]
instructions = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

matches :: Example -> Instruction -> Bool
matches e i = (i a b c (before e)) == (after e) where
    (_:a:b:c:_) = operation e

numMatches :: Example -> Int
numMatches e = length $ filter (matches e) instructions

solve :: ([Example], [[Int]]) -> Int
solve (es, os) = length $ filter (>= 3) $ map numMatches es

main :: IO()
main = interact (show . solve . parse)
