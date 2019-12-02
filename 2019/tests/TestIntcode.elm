module TestIntcode exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Intcode exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Intcode"
        [ describe "MemoryTape"
            [ test "to and from array" <|
                \_ -> [ 1, 2, 3 ] |> tapeFromList |> tapeToList |> Expect.equal [ 1, 2, 3 ]
            , test "shift doesn't change values" <|
                \_ -> [ 1, 2, 3 ] |> tapeFromList |> shiftLeft 2 |> tapeToList |> Expect.equal [ 1, 2, 3 ]
            , test "shift shifts by the right amount" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 2 |> getRel 0 |> Expect.equal (Ok 3)
            , test "two shifts do the right thing" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 2 |> shiftLeft 1 |> tapeToList |> Expect.equal [ 1, 2, 3, 4 ]
            , test "getAbs works for forward value when at base" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> getAbs 2 |> Expect.equal (Ok 3)
            , test "getAbs works for forward value when shifted" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 1 |> getAbs 2 |> Expect.equal (Ok 3)
            , test "getAbs works for backward value" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 3 |> getAbs 2 |> Expect.equal (Ok 3)
            , test "setAbs works for forward value when at base" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> setAbs 2 99 |> tapeToList |> Expect.equal [ 1, 2, 99, 4 ]
            , test "setAbs works for forward value when shifted" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 1 |> setAbs 2 99 |> tapeToList |> Expect.equal [ 1, 2, 99, 4 ]
            , test "setAbs works for backward value" <|
                \_ -> [ 1, 2, 3, 4 ] |> tapeFromList |> shiftLeft 3 |> setAbs 2 99 |> tapeToList |> Expect.equal [ 1, 2, 99, 4 ]
            ]
        , describe "Operations"
            [ test "add sets the target cell" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeAdd |> Result.andThen (getMemory >> getAbs 0) |> Expect.equal (Ok 101)
            , test "add shifts the IP by 4" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeAdd |> Result.andThen (getMemory >> getRel 0) |> Expect.equal (Ok 99)
            , test "mul sets the target cell" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeMul |> Result.andThen (getMemory >> getAbs 0) |> Expect.equal (Ok (99 * 2))
            , test "mul shifts the IP by 4" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeMul |> Result.andThen (getMemory >> getRel 0) |> Expect.equal (Ok 99)
            ]
        , describe "Vm program flow"
            [ test "just an add" <|
                \_ -> runProgram [ 1, 4, 2, 0, 99 ] |> Expect.equal (Ok [ 101, 4, 2, 0, 99 ])
            , test "example 1" <|
                \_ ->
                    runProgram [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
                        |> Expect.equal (Ok [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ])
            , test "example 2" <|
                \_ ->
                    runProgram [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ]
                        |> Expect.equal (Ok [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ])
            ]
        ]
