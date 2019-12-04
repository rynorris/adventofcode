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
    = InProgressB (List Int) Int Int
    | AnswerB Int Int Int


initB : String -> StateB
initB =
    parseInput >> Intcode.tapeToList >> (\code -> InProgressB code 0 0)


patchCode : Int -> Int -> Intcode.MemoryTape -> Intcode.MemoryTape
patchCode n v =
    Intcode.setAbs 1 n >> Intcode.setAbs 2 v


nextGuess : Int -> Int -> ( Int, Int )
nextGuess n v =
    case ( n, v ) of
        ( 99, 99 ) ->
            ( 0, 0 )

        ( _, 99 ) ->
            ( n + 1, 0 )

        ( _, _ ) ->
            ( n, v + 1 )


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB code n v ->
            let
                vm =
                    Intcode.runProgram (code |> Intcode.tapeFromList |> patchCode n v |> Intcode.tapeToList)
            in
            case vm |> Result.andThen (List.head >> Result.fromMaybe "No answer") of
                Ok 19690720 ->
                    ( AnswerB n v (n * 100 + v), True )

                Ok _ ->
                    ( nextGuess n v |> (\( n2, v2 ) -> InProgressB code n2 v2), False )

                Err _ ->
                    ( nextGuess n v |> (\( n2, v2 ) -> InProgressB code n2 v2), False )

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
        , C.adventOfCodeProblemLink 2019 2
        , Html.p []
            [ text "We're getting into custom assembly code early this year!  Looks like they're introducing it very slowly though, just a couple of instructions this time.  Looking forward to where this goes."
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
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "For this part I do a simple brute-force search over all possible values of noun and verb."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day2.elm"
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
                AnswerB n v ans ->
                    div [] [ text ("Done  -- Noun: " ++ String.fromInt n ++ ", Verb: " ++ String.fromInt v ++ ", Answer: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        Exec.Running state _ ->
            case state of
                InProgressB _ n v ->
                    div [] [ text ("Guess -- Noun: " ++ String.fromInt n ++ ", Verb: " ++ String.fromInt v ++ ", Answer: " ++ String.fromInt (n * 100 + v)) ]

                _ ->
                    div [] []

        Exec.Paused state _ ->
            case state of
                InProgressB _ n v ->
                    div [] [ text ("Guess -- Noun: " ++ String.fromInt n ++ ", Verb: " ++ String.fromInt v ++ ", Answer: " ++ String.fromInt (n * 100 + v) ++ " (Paused)") ]

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
