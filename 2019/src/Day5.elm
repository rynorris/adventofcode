module Day5 exposing (Model, Msg, init, initAction, name, update, view)

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
    "Day 5: Sunny with a Chance of Asteroids"



-- Common


parseInput : String -> Intcode.MemoryTape
parseInput =
    String.split "," >> List.map (String.toInt >> Maybe.withDefault 0) >> Intcode.tapeFromList



-- Solve Part A


type StateA
    = InProgressA Intcode.Vm (List String)
    | AnswerA Intcode.Vm (List String)


initA : String -> StateA
initA =
    parseInput >> (\mem -> Intcode.Running mem 0) >> (\vm -> InProgressA vm [])


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA vm console ->
            let
                step =
                    Intcode.intcodeStep vm
            in
            case step of
                Ok nextVm ->
                    case nextVm of
                        Intcode.Halted _ _ ->
                            ( AnswerA nextVm ("HALTED" :: console), True )

                        Intcode.WaitingForInput _ _ _ ->
                            ( InProgressA (Intcode.giveInput 1 nextVm) console, False )

                        Intcode.WaitingToOutput _ _ val ->
                            ( InProgressA (Intcode.takeOutput nextVm) ((val |> String.fromInt) :: console), False )

                        Intcode.Running _ ip ->
                            ( InProgressA nextVm console, False )

                Err txt ->
                    ( AnswerA vm (txt :: console), True )

        _ ->
            ( state, True )



-- Solve Part B


type StateB
    = InProgressB Intcode.Vm (List String)
    | AnswerB Intcode.Vm (List String)


initB : String -> StateB
initB =
    parseInput >> (\mem -> Intcode.Running mem 0) >> (\vm -> InProgressB vm [])


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB vm console ->
            let
                step =
                    Intcode.intcodeStep vm
            in
            case step of
                Ok nextVm ->
                    case nextVm of
                        Intcode.Halted _ _ ->
                            ( AnswerB nextVm ("HALTED" :: console), True )

                        Intcode.WaitingForInput _ _ _ ->
                            ( InProgressB (Intcode.giveInput 5 nextVm) console, False )

                        Intcode.WaitingToOutput _ _ val ->
                            ( InProgressB (Intcode.takeOutput nextVm) ((val |> String.fromInt) :: console), False )

                        _ ->
                            ( InProgressB nextVm console, False )

                Err txt ->
                    ( AnswerB vm (txt :: console), True )

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
    Advent.update 1000 1000



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 5
        , Html.p []
            [ text "A little tricky to get input/output working in this Elm site setup, but we got there in the end.  Not super happy with it, but it works."
            ]
        , Html.p []
            [ text "View the source code for my Intcode interpreter "
            , C.githubLink "on GitHub." "Intcode.elm"
            ]
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.section "Part A"
            [ text "Here we have implemented the Intcode VM and run the given program to completion.  Not forgetting to make the gravity substitutions before starting!"
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                ]
            , viewProgressA model
            ]
        , C.section "Part B"
            [ text "For this part I do a simple brute-force search over all possible values of noun and verb."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                ]
            , viewProgressB model
            ]
        , C.sourceCodeLink "Day5.elm"
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA vm console ->
                    div [] [ viewConsole console ]

                InProgressA vm console ->
                    div [] [ viewConsole console ]

        _ ->
            div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Finished state ->
            case state of
                AnswerB vm console ->
                    div [] [ viewConsole console ]

                InProgressB vm console ->
                    div [] [ viewConsole console ]

        _ ->
            div [] []


viewConsole : List String -> Html Msg
viewConsole lines =
    lines |> List.reverse |> List.map (\s -> div [ class "w-100 code" ] [ text s ]) |> div [ class "w-100 ph2 bg-dark-gray" ]
