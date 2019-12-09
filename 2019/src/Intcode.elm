module Intcode exposing (..)

import Array exposing (Array)
import BigInt exposing (BigInt)


type alias Addr =
    BigInt


type alias Val =
    BigInt


type alias MemoryTape =
    Array Val


tapeFromList : List Val -> MemoryTape
tapeFromList =
    Array.fromList


tapeToList : MemoryTape -> List Val
tapeToList =
    Array.toList


getAbs : Addr -> MemoryTape -> Result String Val
getAbs n =
    Array.get (downcastUnsafe n) >> Result.fromMaybe ("memory index out of bounds: " ++ BigInt.toString n)


getAbsInt : Int -> MemoryTape -> Result String Val
getAbsInt n =
    Array.get n >> Result.fromMaybe ("memory index out of bounds: " ++ String.fromInt n)


setAbs : Addr -> Val -> MemoryTape -> MemoryTape
setAbs n =
    Array.set (downcastUnsafe n)


setAbsInt : Int -> Int -> MemoryTape -> MemoryTape
setAbsInt a n =
    Array.set a (BigInt.fromInt n)


downcastUnsafe : BigInt -> Int
downcastUnsafe =
    BigInt.toString >> String.toInt >> Maybe.withDefault 0


addInt : BigInt -> Int -> BigInt
addInt big x =
    BigInt.add big (BigInt.fromInt x)


type Vm
    = Running MemoryTape Addr
    | Halted MemoryTape Addr
    | WaitingForInput MemoryTape Addr Addr
    | WaitingToOutput MemoryTape Addr Addr


type alias Operation =
    MemoryTape -> Addr -> Result String Vm


type Arg
    = Positional Val
    | Immediate Val


giveInput : Val -> Vm -> Vm
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


resolveArg : MemoryTape -> Arg -> Result String Val
resolveArg tape arg =
    case arg of
        Positional x ->
            getAbs x tape

        Immediate x ->
            Ok x


outputArg : Arg -> Result String Val
outputArg arg =
    case arg of
        Positional x ->
            Ok x

        Immediate x ->
            Ok x


createVm : List Val -> Vm
createVm =
    tapeFromList >> vmFromTape


vmFromTape : MemoryTape -> Vm
vmFromTape tape =
    Running tape (BigInt.fromInt 0)


readProgram : String -> List Val
readProgram =
    String.split "," >> List.map (BigInt.fromIntString >> Maybe.withDefault (BigInt.fromInt 0))


runProgram : List Val -> Result String (List Val)
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


getIp : Vm -> Addr
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
                |> Result.mapError (\s -> "error decoding instruction at IP = " ++ BigInt.toString ip)
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


decodeArg : MemoryTape -> Addr -> Val -> Int -> Result String Arg
decodeArg tape ip opcode ix =
    let
        mode =
            downcastUnsafe opcode // (10 ^ (ix + 1)) |> modBy 10

        raw =
            getAbs (BigInt.add ip (BigInt.fromInt ix)) tape
    in
    case mode of
        0 ->
            Result.map Positional raw

        1 ->
            Result.map Immediate raw

        _ ->
            Err ("Unknown addressing mode: " ++ String.fromInt mode)


decodeInstruction : MemoryTape -> Addr -> Result String Instruction
decodeInstruction tape ip =
    getAbs ip tape
        |> Result.andThen
            (\opcode ->
                let
                    arg =
                        decodeArg tape ip opcode
                in
                case modBy 100 (downcastUnsafe opcode) of
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


interpretInstruction : MemoryTape -> Addr -> Instruction -> Result String Operation
interpretInstruction tape ip inst =
    let
        resolve =
            resolveArg tape
    in
    case inst of
        Halt ->
            Ok opHalt

        Add a b c ->
            Result.map3 opAdd (resolve a) (resolve b) (outputArg c)

        Mul a b c ->
            Result.map3 opMul (resolve a) (resolve b) (outputArg c)

        Inp a ->
            Result.map opInp (outputArg a)

        Out a ->
            Result.map opOut (resolve a)

        Jit a b ->
            Result.map2 opJit (resolve a) (resolve b)

        Jif a b ->
            Result.map2 opJif (resolve a) (resolve b)

        Lt a b c ->
            Result.map3 opLt (resolve a) (resolve b) (outputArg c)

        Eq a b c ->
            Result.map3 opEq (resolve a) (resolve b) (outputArg c)


executeInstruction : Instruction -> MemoryTape -> Addr -> Result String Vm
executeInstruction inst tape ip =
    interpretInstruction tape ip inst |> Result.andThen (\op -> op tape ip) |> Result.mapError (\s -> "encountered error at IP = " ++ BigInt.toString ip ++ ": " ++ s)


opHalt : Operation
opHalt tape ip =
    Ok (Halted tape ip)


opAdd : Val -> Val -> Val -> Operation
opAdd =
    binaryOp BigInt.add


opMul : Val -> Val -> Val -> Operation
opMul =
    binaryOp BigInt.mul


binaryOp : (Val -> Val -> Val) -> Val -> Val -> Val -> Operation
binaryOp calc a1 a2 tgt tape ip =
    Ok (Running (setAbs tgt (calc a1 a2) tape) (addInt ip 4))


opInp : Addr -> Operation
opInp tgt tape ip =
    Ok (WaitingForInput tape (addInt ip 2) tgt)


opOut : Val -> Operation
opOut val tape ip =
    Ok (WaitingToOutput tape (addInt ip 2) val)


opJit : Val -> Val -> Operation
opJit =
    condJump (\x -> BigInt.compare x (BigInt.fromInt 0) /= EQ)


opJif : Val -> Val -> Operation
opJif =
    condJump (\x -> BigInt.compare x (BigInt.fromInt 0) == EQ)


condJump : (Val -> Bool) -> Val -> Addr -> Operation
condJump cond val tgt tape ip =
    if cond val then
        Ok (Running tape tgt)

    else
        Ok (Running tape (addInt ip 3))


opLt : Val -> Val -> Val -> Operation
opLt =
    condSet BigInt.lt


opEq : Val -> Val -> Val -> Operation
opEq =
    condSet (\x y -> BigInt.compare x y == EQ)


condSet : (Val -> Val -> Bool) -> Val -> Val -> Addr -> Operation
condSet cond a1 a2 tgt tape ip =
    let
        val =
            if cond a1 a2 then
                BigInt.fromInt 1

            else
                BigInt.fromInt 0
    in
    Ok (Running (setAbs tgt val tape) (addInt ip 4))
