import Day16

import Advent.IndexedList

import Debug.Trace

data Operation = Operation { instruction :: String, argA :: Int, argB :: Int, argC :: Int } deriving (Show, Eq, Ord)

type Program = IndexedList Operation

data CPU = CPU { registers :: Registers, program :: Program, pointer :: Int, pointerReg :: Int } deriving (Show, Eq, Ord)

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

parse :: String -> CPU
parse s = newCPU 6 pointerReg program where
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
