module Intcode exposing (..)


type alias MemoryTape =
    { before : List Int
    , after : List Int
    }


tapeFromList : List Int -> MemoryTape
tapeFromList xs =
    { before = [], after = xs }


tapeToList : MemoryTape -> List Int
tapeToList tape =
    List.append (List.reverse tape.before) tape.after


getRel : Int -> MemoryTape -> Result String Int
getRel ix tape =
    tape.after |> List.drop ix |> List.head |> Result.fromMaybe "index out of bounds"


setRel : MemoryTape -> Int -> Int -> MemoryTape
setRel tape ix val =
    let
        first =
            List.take ix tape.after

        second =
            List.drop (ix + 1) tape.after
    in
    { tape | after = List.append first (val :: second) }


getAbs : Int -> MemoryTape -> Result String Int
getAbs ix tape =
    if ix < List.length tape.before then
        tape.before |> List.drop (List.length tape.before - ix - 1) |> List.head |> Result.fromMaybe "index out of bounds"

    else
        tape.after |> List.drop (ix - List.length tape.before) |> List.head |> Result.fromMaybe "index out of bounds"


setAbs : Int -> Int -> MemoryTape -> MemoryTape
setAbs ix val tape =
    if ix < List.length tape.before then
        let
            first =
                List.take (List.length tape.before - ix - 1) tape.before

            second =
                List.drop (List.length tape.before - ix) tape.before
        in
        { tape | before = List.append first (val :: second) }

    else
        let
            first =
                List.take (ix - List.length tape.before) tape.after

            second =
                List.drop (ix - List.length tape.before + 1) tape.after
        in
        { tape | after = List.append first (val :: second) }


shiftLeft : Int -> MemoryTape -> MemoryTape
shiftLeft n tape =
    { before = List.append (List.reverse (List.take n tape.after)) tape.before
    , after = List.drop n tape.after
    }


type Vm
    = Running MemoryTape
    | Halted MemoryTape


type alias Operation =
    MemoryTape -> Result String MemoryTape


createVm : List Int -> Vm
createVm =
    tapeFromList >> Running


runProgram : List Int -> Result String (List Int)
runProgram xs =
    tapeFromList xs |> Running |> runToCompletion |> Result.map (getMemory >> tapeToList)


runToCompletion : Vm -> Result String Vm
runToCompletion vm =
    case vm of
        Running _ ->
            intcodeStep vm |> Result.andThen runToCompletion

        Halted _ ->
            Ok vm


getMemory : Vm -> MemoryTape
getMemory vm =
    case vm of
        Running mem ->
            mem

        Halted mem ->
            mem


intcodeStep : Vm -> Result String Vm
intcodeStep vm =
    case vm of
        Running tape ->
            case getRel 0 tape of
                Ok 99 ->
                    Ok (Halted tape)

                Ok 1 ->
                    doOp intcodeAdd vm

                Ok 2 ->
                    doOp intcodeMul vm

                Ok x ->
                    Err ("Unknown opcode: " ++ String.fromInt x)

                Err msg ->
                    Err msg

        Halted _ ->
            Ok vm


doOp : Operation -> Vm -> Result String Vm
doOp op vm =
    case vm of
        Running tape ->
            op tape |> Result.andThen (\new -> Ok (Running new))

        _ ->
            Ok vm


binaryOp : (Int -> Int -> Int) -> Operation
binaryOp calc tape =
    let
        v1 =
            getRel 1 tape |> Result.andThen (\ix -> getAbs ix tape)

        v2 =
            getRel 2 tape |> Result.andThen (\ix -> getAbs ix tape)

        addr =
            getRel 3 tape
    in
    Result.map3 (\x1 x2 a -> setAbs a (calc x1 x2) tape |> shiftLeft 4) v1 v2 addr


intcodeAdd : Operation
intcodeAdd =
    binaryOp (+)


intcodeMul : Operation
intcodeMul =
    binaryOp (*)
