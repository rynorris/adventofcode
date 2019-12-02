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
    Vm -> Result String Vm


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
            case getAbs ip tape of
                Ok 99 ->
                    Ok (Halted tape ip)

                Ok 1 ->
                    doOp intcodeAdd vm

                Ok 2 ->
                    doOp intcodeMul vm

                Ok x ->
                    Err ("Unknown opcode: " ++ String.fromInt x)

                Err msg ->
                    Err msg

        Halted _ _ ->
            Ok vm


doOp : Operation -> Vm -> Result String Vm
doOp op vm =
    case vm of
        Running _ _ ->
            op vm

        _ ->
            Ok vm


binaryOp : (Int -> Int -> Int) -> Operation
binaryOp calc vm =
    case vm of
        Running tape ip ->
            let
                v1 =
                    getAbs (ip + 1) tape |> Result.andThen (\ix -> getAbs ix tape)

                v2 =
                    getAbs (ip + 2) tape |> Result.andThen (\ix -> getAbs ix tape)

                addr =
                    getAbs (ip + 3) tape
            in
            Result.map3 (\x1 x2 a -> Running (setAbs a (calc x1 x2) tape) (ip + 4)) v1 v2 addr

        Halted _ _ ->
            Ok vm


intcodeAdd : Operation
intcodeAdd =
    binaryOp (+)


intcodeMul : Operation
intcodeMul =
    binaryOp (*)
