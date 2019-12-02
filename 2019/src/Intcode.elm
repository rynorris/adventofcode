module Intcode exposing (..)

import Array exposing (Array)


type alias MemoryTape =
    Array Int


tapeFromList : List Int -> MemoryTape
tapeFromList =
    Array.fromList


tapeToList : MemoryTape -> List Int
tapeToList =
    Array.toList


getAbs : Int -> MemoryTape -> Result String Int
getAbs n =
    Array.get n >> Result.fromMaybe "index out of bounds"


setAbs : Int -> Int -> MemoryTape -> MemoryTape
setAbs =
    Array.set


type Vm
    = Running MemoryTape Int
    | Halted MemoryTape Int


type alias Operation =
    MemoryTape -> Int -> Result String Vm


createVm : List Int -> Vm
createVm code =
    Running (tapeFromList code) 0


runProgram : List Int -> Result String (List Int)
runProgram xs =
    createVm xs |> runToCompletion |> Result.map (getMemory >> tapeToList)


runToCompletion : Vm -> Result String Vm
runToCompletion vm =
    case vm of
        Running _ _ ->
            intcodeStep vm |> Result.andThen runToCompletion

        Halted _ _ ->
            Ok vm


getMemory : Vm -> MemoryTape
getMemory vm =
    case vm of
        Running mem _ ->
            mem

        Halted mem _ ->
            mem


getIp : Vm -> Int
getIp vm =
    case vm of
        Running _ ip ->
            ip

        Halted _ ip ->
            ip


intcodeStep : Vm -> Result String Vm
intcodeStep vm =
    case vm of
        Running tape ip ->
            decodeInstruction tape ip |> Result.andThen (\inst -> executeInstruction inst tape ip)

        Halted _ _ ->
            Ok vm



-- Define Instructions


type Instruction
    = Halt
    | Add Int Int Int
    | Mul Int Int Int


decodeInstruction : MemoryTape -> Int -> Result String Instruction
decodeInstruction tape ip =
    let
        arg =
            \n -> getAbs (ip + n) tape
    in
    case getAbs ip tape of
        Ok 99 ->
            Ok Halt

        Ok 1 ->
            Result.map3 Add (arg 1) (arg 2) (arg 3)

        Ok 2 ->
            Result.map3 Mul (arg 1) (arg 2) (arg 3)

        Ok x ->
            Err ("unknown opcode: " ++ String.fromInt x)

        Err s ->
            Err ("failed to read opcode: " ++ s)


interpretInstruction : Instruction -> Operation
interpretInstruction inst =
    case inst of
        Halt ->
            opHalt

        Add a b c ->
            opAdd a b c

        Mul a b c ->
            opMul a b c


executeInstruction : Instruction -> MemoryTape -> Int -> Result String Vm
executeInstruction inst tape ip =
    interpretInstruction inst |> (\op -> op tape ip)


opHalt : Operation
opHalt tape ip =
    Ok (Halted tape ip)


opAdd : Int -> Int -> Int -> Operation
opAdd =
    binaryOp (+)


opMul : Int -> Int -> Int -> Operation
opMul =
    binaryOp (*)


binaryOp : (Int -> Int -> Int) -> Int -> Int -> Int -> Operation
binaryOp calc a1 a2 tgt tape ip =
    let
        x1 =
            getAbs a1 tape

        x2 =
            getAbs a2 tape
    in
    Result.map2 (\x y -> Running (setAbs tgt (calc x y) tape) (ip + 4)) x1 x2
