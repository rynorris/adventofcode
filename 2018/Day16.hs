module Day16 where

import Data.Bits

data Example = Example { before :: Registers, operation :: [Int], after :: Registers } deriving (Show, Eq)
type Registers = [Int]
type Instruction = Int -> Int -> Int -> Registers -> Registers

setRegister :: Int -> Int -> Registers -> Registers
setRegister r x rs = pre ++ (x:tail) where
    (pre, t:tail) = splitAt r rs

getRegister :: Int -> Registers -> Int
getRegister r rs = rs !! r

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

