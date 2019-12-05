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
    Array.get n >> Result.fromMaybe ("memory index out of bounds: " ++ String.fromInt n)


setAbs : Int -> Int -> MemoryTape -> MemoryTape
setAbs =
    Array.set


type Vm
    = Running MemoryTape Int
    | Halted MemoryTape Int
    | WaitingForInput MemoryTape Int Int
    | WaitingToOutput MemoryTape Int Int


type alias Operation =
    MemoryTape -> Int -> Result String Vm


type Arg
    = Positional Int
    | Immediate Int


giveInput : Int -> Vm -> Vm
giveInput val vm =
    case vm of
        WaitingForInput tape ip tgt ->
            Running (setAbs tgt val tape) ip

        _ ->
            vm


takeOutput : Vm -> Vm
takeOutput vm =
    case vm of
        WaitingToOutput tape ip _ ->
            Running tape ip

        _ ->
            vm


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

        _ ->
            Ok vm


getMemory : Vm -> MemoryTape
getMemory vm =
    case vm of
        Running mem _ ->
            mem

        Halted mem _ ->
            mem

        WaitingForInput mem _ _ ->
            mem

        WaitingToOutput mem _ _ ->
            mem


getIp : Vm -> Int
getIp vm =
    case vm of
        Running _ ip ->
            ip

        Halted _ ip ->
            ip

        WaitingForInput _ ip _ ->
            ip

        WaitingToOutput _ ip _ ->
            ip


intcodeStep : Vm -> Result String Vm
intcodeStep vm =
    case vm of
        Running tape ip ->
            decodeInstruction tape ip
                |> Result.mapError (\s -> "error decoding instruction at IP = " ++ String.fromInt ip)
                |> Result.andThen (\inst -> executeInstruction inst tape ip)

        _ ->
            Ok vm



-- Define Instructions


type Instruction
    = Halt
    | Add Arg Arg Arg
    | Mul Arg Arg Arg
    | Inp Arg
    | Out Arg
    | Jit Arg Arg
    | Jif Arg Arg
    | Lt Arg Arg Arg
    | Eq Arg Arg Arg


decodeArg : MemoryTape -> Int -> Int -> Int -> Result String Arg
decodeArg tape ip opcode ix =
    let
        mode =
            opcode // (10 ^ (ix + 1)) |> modBy 10

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

                    3 ->
                        Result.map Inp (arg 1)

                    4 ->
                        Result.map Out (arg 1)

                    5 ->
                        Result.map2 Jit (arg 1) (arg 2)

                    6 ->
                        Result.map2 Jif (arg 1) (arg 2)

                    7 ->
                        Result.map3 Lt (arg 1) (arg 2) (arg 3)

                    8 ->
                        Result.map3 Eq (arg 1) (arg 2) (arg 3)

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

        Inp a ->
            Result.map opInp (rawArg a)

        Out a ->
            Result.map opOut (resolve a)

        Jit a b ->
            Result.map2 opJit (resolve a) (resolve b)

        Jif a b ->
            Result.map2 opJif (resolve a) (resolve b)

        Lt a b c ->
            Result.map3 opLt (resolve a) (resolve b) (rawArg c)

        Eq a b c ->
            Result.map3 opEq (resolve a) (resolve b) (rawArg c)


executeInstruction : Instruction -> MemoryTape -> Int -> Result String Vm
executeInstruction inst tape ip =
    interpretInstruction tape ip inst |> Result.andThen (\op -> op tape ip) |> Result.mapError (\s -> "encountered error at IP = " ++ String.fromInt ip ++ ": " ++ s)


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


opInp : Int -> Operation
opInp tgt tape ip =
    Ok (WaitingForInput tape (ip + 2) tgt)


opOut : Int -> Operation
opOut val tape ip =
    Ok (WaitingToOutput tape (ip + 2) val)


opJit : Int -> Int -> Operation
opJit =
    condJump (\x -> x /= 0)


opJif : Int -> Int -> Operation
opJif =
    condJump (\x -> x == 0)


condJump : (Int -> Bool) -> Int -> Int -> Operation
condJump cond val tgt tape ip =
    if cond val then
        Ok (Running tape tgt)

    else
        Ok (Running tape (ip + 3))


opLt : Int -> Int -> Int -> Operation
opLt =
    condSet (\x y -> x < y)


opEq : Int -> Int -> Int -> Operation
opEq =
    condSet (\x y -> x == y)


condSet : (Int -> Int -> Bool) -> Int -> Int -> Int -> Operation
condSet cond a1 a2 tgt tape ip =
    let
        val =
            if cond a1 a2 then
                1

            else
                0
    in
    Ok (Running (setAbs tgt val tape) (ip + 4))
