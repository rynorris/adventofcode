module TestIntcode exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Intcode exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Intcode"
        [ describe "Operations"
            [ test "add sets the target cell" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeAdd |> Result.andThen (getMemory >> getAbs 0) |> Expect.equal (Ok 101)
            , test "add shifts the IP by 4" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeAdd |> Result.map getIp |> Expect.equal (Ok 4)
            , test "mul sets the target cell" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeMul |> Result.andThen (getMemory >> getAbs 0) |> Expect.equal (Ok (99 * 2))
            , test "mul shifts the IP by 4" <|
                \_ -> [ 1, 4, 2, 0, 99 ] |> createVm |> doOp intcodeMul |> Result.map getIp |> Expect.equal (Ok 4)
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
