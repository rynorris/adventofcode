module Day2 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Dict exposing (Dict)
import Exec
import Grid exposing (Grid)
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Intcode
import Set exposing (Set)


name =
    "Day 2: 1202 Program Alarm"


exampleInputs : List Advent.ExampleInput
exampleInputs =
    [ { name = "Example 1", input = "1,9,10,3,2,3,11,0,99,30,40,50" }
    ]



-- Common


parseInput : String -> Intcode.MemoryTape
parseInput =
    String.split "," >> List.map (String.toInt >> Maybe.withDefault 0) >> Intcode.tapeFromList



-- Solve Part A


type StateA
    = InProgressA Intcode.Vm
    | AnswerA Intcode.Vm


initA : String -> StateA
initA =
    parseInput >> Intcode.setAbs 1 12 >> Intcode.setAbs 2 2 >> (\mem -> Intcode.Running mem 0) >> InProgressA


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA vm ->
            let
                step =
                    Intcode.intcodeStep vm
            in
            case step of
                Ok nextVm ->
                    case nextVm of
                        Intcode.Running _ _ ->
                            ( InProgressA nextVm, False )

                        Intcode.Halted _ _ ->
                            ( AnswerA nextVm, True )

                Err _ ->
                    ( AnswerA vm, True )

        _ ->
            ( state, True )



-- Solve Part B


type StateB
    = InProgressB (List Int)
    | AnswerB Int


initB : String -> StateB
initB =
    parseInput >> Intcode.tapeToList >> InProgressB


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB nums ->
            ( AnswerB (List.map identity nums |> List.sum), True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day2.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 1
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.loadExampleButtons exampleInputs
        , C.section "Part A"
            [ text "Here we have implemented the Intcode VM and run the given program to completion.  Not forgetting to make the gravity substitutions before starting!"
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "Here we define a recursive version of the original function to take into account the fuel mass as well.  Then as before, map and sum."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day1.elm"
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA vm ->
                    div []
                        [ div [ class "f3" ] [ text ("Answer is: " ++ (getMemoryAt 0 vm |> resultToString identity String.fromInt)) ]
                        , div [ class "h5 overflow-auto" ] [ viewMemoryTape (Intcode.getMemory vm) ]
                        ]

                InProgressA vm ->
                    div [ class "h5 overflow-auto" ] [ viewMemoryTape (Intcode.getMemory vm) ]

        _ ->
            div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Finished state ->
            case state of
                AnswerB ans ->
                    div [] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        _ ->
            div [] []


resultToString : (a -> String) -> (b -> String) -> Result a b -> String
resultToString mapA mapB res =
    case res of
        Err err ->
            mapA err

        Ok val ->
            mapB val


getMemoryAt : Int -> Intcode.Vm -> Result String Int
getMemoryAt n vm =
    vm |> Intcode.getMemory |> Intcode.getAbs n


viewMemoryTape : Intcode.MemoryTape -> Html Msg
viewMemoryTape =
    Intcode.tapeToList >> listToGrid 10 >> Grid.drawHtml (Maybe.map (String.fromInt >> text) >> Maybe.withDefault (div [] []))


listToGrid : Int -> List Int -> Grid Int
listToGrid w xs =
    xs
        |> List.indexedMap (\ix x -> ( ( modBy w ix, ix // w ), x ))
        |> Dict.fromList
