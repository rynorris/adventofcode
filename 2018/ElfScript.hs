module ElfScript where

import Advent.IndexedList

import Data.Bits

data Example = Example { before :: Registers, operation :: [Int], after :: Registers } deriving (Show, Eq)
type Registers = [Int]
type Instruction = Int -> Int -> Int -> Registers -> Registers
data Operation = Operation { instruction :: String, argA :: Int, argB :: Int, argC :: Int } deriving (Show, Eq, Ord)
type Program = IndexedList Operation
data CPU = CPU { registers :: Registers, program :: Program, pointer :: Int, pointerReg :: Int } deriving (Show, Eq, Ord)

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

newCPU :: Int -> Int -> Program -> CPU
newCPU rs pr p = CPU (take rs $ repeat 0) p 0 pr

parseInstruction :: String -> Instruction
parseInstruction "addr" = addr
parseInstruction "addi" = addi
parseInstruction "mulr" = mulr
parseInstruction "muli" = muli
parseInstruction "banr" = banr
parseInstruction "bani" = bani
parseInstruction "borr" = borr
parseInstruction "bori" = bori
parseInstruction "setr" = setr
parseInstruction "seti" = seti
parseInstruction "gtir" = gtir
parseInstruction "gtri" = gtri
parseInstruction "gtrr" = gtrr
parseInstruction "eqir" = eqir
parseInstruction "eqri" = eqri
parseInstruction "eqrr" = eqrr

parseOperation :: String -> Operation
parseOperation s = Operation i (read a) (read b) (read c) where
    (i:a:b:c:_) = words s

parseProgram :: [String] -> IndexedList Operation
parseProgram = foldl append emptyList . map parseOperation

parseCPU :: String -> CPU
parseCPU s = newCPU 6 pointerReg program where
    pointerReg = read $ head $ drop 1 $ words l
    program = parseProgram ls
    (l:ls) = lines s

pointerToReg :: CPU -> CPU
pointerToReg (CPU rs prog po pr) = CPU (setRegister pr po rs) prog po pr

regToPointer :: CPU -> CPU
regToPointer (CPU rs prog po pr) = CPU rs prog (getRegister pr rs) pr

incPointer :: CPU -> CPU
incPointer (CPU rs prog po pr) = CPU rs prog (po+1) pr

executeOperation :: Operation -> CPU -> CPU
executeOperation (Operation i a b c) (CPU rs prog po pr) = CPU (f a b c rs) prog po pr where
    f = parseInstruction i

step :: CPU -> CPU
step cpu@(CPU _ prog po _) = incPointer $ regToPointer $ executeOperation (getIndex prog po) $ pointerToReg cpu where

shouldBreak :: CPU -> Bool
shouldBreak (CPU _ prog po _) = po >= listLength prog

