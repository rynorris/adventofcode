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


type Arg
    = Positional Int
    | Immediate Int


resolveArg : MemoryTape -> Arg -> Result String Int
resolveArg tape arg =
    case arg of
        Positional x ->
            getAbs x tape

        Immediate x ->
            Ok x


rawArg : Arg -> Result String Int
rawArg arg =
    case arg of
        Positional x ->
            Ok x

        Immediate x ->
            Ok x


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
    | Add Arg Arg Arg
    | Mul Arg Arg Arg


decodeArg : MemoryTape -> Int -> Int -> Int -> Result String Arg
decodeArg tape ip opcode ix =
    let
        mode =
            opcode // (10 ^ (ix + 2)) |> modBy 10

        raw =
            getAbs (ip + ix) tape
    in
    case mode of
        0 ->
            Result.map Positional raw

        1 ->
            Result.map Immediate raw

        _ ->
            Err ("Unknown addressing mode: " ++ String.fromInt mode)


decodeInstruction : MemoryTape -> Int -> Result String Instruction
decodeInstruction tape ip =
    getAbs ip tape
        |> Result.andThen
            (\opcode ->
                let
                    arg =
                        decodeArg tape ip opcode
                in
                case modBy 100 opcode of
                    99 ->
                        Ok Halt

                    1 ->
                        Result.map3 Add (arg 1) (arg 2) (arg 3)

                    2 ->
                        Result.map3 Mul (arg 1) (arg 2) (arg 3)

                    x ->
                        Err ("unknown opcode: " ++ String.fromInt x)
            )


interpretInstruction : MemoryTape -> Int -> Instruction -> Result String Operation
interpretInstruction tape ip inst =
    let
        resolve =
            resolveArg tape
    in
    case inst of
        Halt ->
            Ok opHalt

        Add a b c ->
            Result.map3 opAdd (resolve a) (resolve b) (rawArg c)

        Mul a b c ->
            Result.map3 opMul (resolve a) (resolve b) (rawArg c)


executeInstruction : Instruction -> MemoryTape -> Int -> Result String Vm
executeInstruction inst tape ip =
    interpretInstruction tape ip inst |> Result.andThen (\op -> op tape ip)


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
    Ok (Running (setAbs tgt (calc a1 a2) tape) (ip + 4))
