import Data.Bits

data Example = Example { before :: Registers, operation :: [Int], after :: Registers } deriving (Show, Eq)
type Registers = [Int]
type Instruction = Int -> Int -> Int -> Registers -> Registers

setRegister :: Int -> Int -> Registers -> Registers
setRegister r x rs = pre ++ (x:tail) where
    (pre, t:tail) = splitAt r rs

getRegister :: Int -> Registers -> Int
getRegister r rs = rs !! r

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

instr :: (Int -> Int -> Int -> Int -> Int) -> Instruction
instr f = g where
    g a b c rs = setRegister c (f a b (getRegister a rs) (getRegister b rs)) rs

addr :: Instruction
addr = instr go where
    go a b a' b' = a' + b'

addi :: Instruction
addi = instr go where
    go a b a' b' = a' + b

mulr :: Instruction
mulr = instr go where
    go a b a' b' = a' * b'

muli :: Instruction
muli = instr go where
    go a b a' b' = a' * b

banr :: Instruction
banr = instr go where
    go a b a' b' = (.&.) a' b'

bani :: Instruction
bani = instr go where
    go a b a' b' = (.&.) a' b

borr :: Instruction
borr = instr go where
    go a b a' b' = (.|.) a' b'

bori :: Instruction
bori = instr go where
    go a b a' b' = (.|.) a' b

setr :: Instruction
setr = instr go where
    go a b a' b' = a'

seti :: Instruction
seti = instr go where
    go a b a' b' = a

gtir :: Instruction
gtir = instr go where
    go a b a' b' = if a > b' then 1 else 0

gtri :: Instruction
gtri = instr go where
    go a b a' b' = if a' > b then 1 else 0

gtrr :: Instruction
gtrr = instr go where
    go a b a' b' = if a' > b' then 1 else 0

eqir :: Instruction
eqir = instr go where
    go a b a' b' = if a == b' then 1 else 0

eqri :: Instruction
eqri = instr go where
    go a b a' b' = if a' == b then 1 else 0

eqrr :: Instruction
eqrr = instr go where
    go a b a' b' = if a' == b' then 1 else 0

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
