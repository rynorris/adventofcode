module Day2 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Exec
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Day 2: 1202 Program Alarm"


exampleInputs : List Advent.ExampleInput
exampleInputs =
    [ { name = "Example 1", input = "1,9,10,3,2,3,11,0,99,30,40,50" }
    ]



-- Common


parseInput : String -> List Int
parseInput =
    String.split "," >> List.map (String.toInt >> Maybe.withDefault 0)



-- Solve Part A


type StateA
    = InProgressA (List Int)
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> InProgressA


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA nums ->
            ( AnswerA (List.map identity nums |> List.sum), True )

        _ ->
            ( state, True )



-- Solve Part B


fuelRec : Int -> Int
fuelRec m =
    case identity m of
        0 ->
            0

        f ->
            f + fuelRec f


type StateB
    = InProgressB (List Int)
    | AnswerB Int


initB : String -> StateB
initB =
    parseInput >> InProgressB


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB nums ->
            ( AnswerB (List.map fuelRec nums |> List.sum), True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day1.elm"


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
            [ text "Here we simply map the formula over the list and sum the results."
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
                AnswerA ans ->
                    div [] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

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
