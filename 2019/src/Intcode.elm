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
getAbs n tape =
    if BigInt.gt n (BigInt.fromInt (Array.length tape)) then
        Ok (BigInt.fromInt 0)

    else
        tape |> Array.get (downcastUnsafe n) |> Result.fromMaybe ("memory index out of bounds: " ++ BigInt.toString n)


getAbsInt : Int -> MemoryTape -> Result String Val
getAbsInt =
    BigInt.fromInt >> getAbs


setAbs : Addr -> Val -> MemoryTape -> MemoryTape
setAbs n val =
    ensureAddr n >> Array.set (downcastUnsafe n) val


ensureAddr : Addr -> MemoryTape -> MemoryTape
ensureAddr n tape =
    let
        shortage =
            BigInt.add (BigInt.sub (BigInt.fromInt (Array.length tape)) n) (BigInt.fromInt 1)
    in
    if BigInt.gt shortage (BigInt.fromInt 0) then
        Array.append tape (Array.repeat (downcastUnsafe shortage) (BigInt.fromInt 0))

    else
        tape


setAbsInt : Int -> Int -> MemoryTape -> MemoryTape
setAbsInt a n =
    setAbs (BigInt.fromInt a) (BigInt.fromInt n)


downcastUnsafe : BigInt -> Int
downcastUnsafe =
    BigInt.toString >> String.toInt >> Maybe.withDefault 0


addInt : BigInt -> Int -> BigInt
addInt big x =
    BigInt.add big (BigInt.fromInt x)


type Vm
    = Running State
    | Halted State
    | WaitingForInput State Addr
    | WaitingToOutput State Addr


type alias State =
    { memory : MemoryTape
    , ip : Addr
    , rel : Addr
    }


type alias Operation =
    State -> Result String Vm


type Arg
    = Positional Val
    | Immediate Val


giveInput : Val -> Vm -> Vm
giveInput val vm =
    case vm of
        WaitingForInput state tgt ->
            Running { state | memory = setAbs tgt val state.memory }

        _ ->
            vm


takeOutput : Vm -> Vm
takeOutput vm =
    case vm of
        WaitingToOutput state _ ->
            Running state

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
    Running { memory = tape, ip = BigInt.fromInt 0, rel = BigInt.fromInt 0 }


readProgram : String -> List Val
readProgram =
    String.split "," >> List.map (BigInt.fromIntString >> Maybe.withDefault (BigInt.fromInt 0))


runProgram : List Val -> Result String (List Val)
runProgram xs =
    createVm xs |> runToCompletion |> Result.map (getMemory >> tapeToList)


runToCompletion : Vm -> Result String Vm
runToCompletion vm =
    case vm of
        Running _ ->
            intcodeStep vm |> Result.andThen runToCompletion

        _ ->
            Ok vm


getMemory : Vm -> MemoryTape
getMemory vm =
    case vm of
        Running state ->
            state.memory

        Halted state ->
            state.memory

        WaitingForInput state _ ->
            state.memory

        WaitingToOutput state _ ->
            state.memory


getIp : Vm -> Addr
getIp vm =
    case vm of
        Running state ->
            state.ip

        Halted state ->
            state.ip

        WaitingForInput state _ ->
            state.ip

        WaitingToOutput state _ ->
            state.ip


intcodeStep : Vm -> Result String Vm
intcodeStep vm =
    case vm of
        Running state ->
            decodeInstruction state
                |> Result.mapError (\s -> "error decoding instruction at IP = " ++ BigInt.toString state.ip)
                |> Result.andThen (\inst -> executeInstruction inst state)

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


decodeInstruction : State -> Result String Instruction
decodeInstruction state =
    getAbs state.ip state.memory
        |> Result.andThen
            (\opcode ->
                let
                    arg =
                        decodeArg state.memory state.ip opcode
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


interpretInstruction : State -> Instruction -> Result String Operation
interpretInstruction state inst =
    let
        resolve =
            resolveArg state.memory
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


executeInstruction : Instruction -> State -> Result String Vm
executeInstruction inst state =
    interpretInstruction state inst |> Result.andThen (\op -> op state) |> Result.mapError (\s -> "encountered error at IP = " ++ BigInt.toString state.ip ++ ": " ++ s)


opHalt : Operation
opHalt state =
    Ok (Halted state)


opAdd : Val -> Val -> Val -> Operation
opAdd =
    binaryOp BigInt.add


opMul : Val -> Val -> Val -> Operation
opMul =
    binaryOp BigInt.mul


binaryOp : (Val -> Val -> Val) -> Val -> Val -> Val -> Operation
binaryOp calc a1 a2 tgt state =
    Ok (Running { state | memory = setAbs tgt (calc a1 a2) state.memory, ip = addInt state.ip 4 })


opInp : Addr -> Operation
opInp tgt state =
    Ok (WaitingForInput { state | ip = addInt state.ip 2 } tgt)


opOut : Val -> Operation
opOut val state =
    Ok (WaitingToOutput { state | ip = addInt state.ip 2 } val)


opJit : Val -> Val -> Operation
opJit =
    condJump (\x -> BigInt.compare x (BigInt.fromInt 0) /= EQ)


opJif : Val -> Val -> Operation
opJif =
    condJump (\x -> BigInt.compare x (BigInt.fromInt 0) == EQ)


condJump : (Val -> Bool) -> Val -> Addr -> Operation
condJump cond val tgt state =
    if cond val then
        Ok (Running { state | ip = tgt })

    else
        Ok (Running { state | ip = addInt state.ip 3 })


opLt : Val -> Val -> Val -> Operation
opLt =
    condSet BigInt.lt


opEq : Val -> Val -> Val -> Operation
opEq =
    condSet (\x y -> BigInt.compare x y == EQ)


condSet : (Val -> Val -> Bool) -> Val -> Val -> Addr -> Operation
condSet cond a1 a2 tgt state =
    let
        val =
            if cond a1 a2 then
                BigInt.fromInt 1

            else
                BigInt.fromInt 0
    in
    Ok (Running { state | memory = setAbs tgt val state.memory, ip = addInt state.ip 4 })
