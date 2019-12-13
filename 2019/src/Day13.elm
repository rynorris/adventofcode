module Day13 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import BigInt exposing (BigInt)
import Components as C
import Dict
import Exec
import Grid exposing (Grid)
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class, style)
import Html.Events exposing (onClick)
import Intcode
import List
import Set exposing (Set)


name =
    "Day 13: Care Package"



-- Common


parseInput : String -> Intcode.MemoryTape
parseInput =
    Intcode.readProgram >> Intcode.tapeFromList



-- Solve Part A


type Entity
    = Wall
    | Block
    | Paddle
    | Ball


toEntity : Int -> Maybe Entity
toEntity v =
    case v of
        1 ->
            Just Wall

        2 ->
            Just Block

        3 ->
            Just Paddle

        4 ->
            Just Ball

        _ ->
            Nothing


updateScreen : Int -> Int -> Maybe Entity -> Grid Entity -> Grid Entity
updateScreen x y me screen =
    case me of
        Just e ->
            Dict.insert ( x, y ) e screen

        Nothing ->
            Dict.remove ( x, y ) screen


type StateA
    = InProgressA Intcode.Vm (Grid Entity) (List Int)
    | ErrorA String
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> Intcode.vmFromTape >> (\vm -> InProgressA vm Dict.empty [])


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA vm screen buf ->
            case vm of
                Intcode.Halted _ ->
                    ( AnswerA (screen |> Dict.values |> List.filter (\x -> x == Block) |> List.length), True )

                Intcode.Running _ ->
                    case Intcode.intcodeStep vm of
                        Ok next ->
                            ( InProgressA next screen buf, False )

                        Err txt ->
                            ( ErrorA txt, True )

                Intcode.WaitingToOutput _ val ->
                    let
                        newBuf =
                            Intcode.downcastUnsafe val :: buf
                    in
                    case newBuf of
                        v :: y :: x :: rest ->
                            ( InProgressA (Intcode.takeOutput vm) (updateScreen x y (toEntity v) screen) [], False )

                        _ ->
                            ( InProgressA (Intcode.takeOutput vm) screen newBuf, False )

                _ ->
                    ( InProgressA vm screen buf, False )

        _ ->
            ( state, True )



-- Solve Part B


findEntity : Entity -> Grid Entity -> Maybe Grid.Point
findEntity e grid =
    grid |> Dict.filter (\k v -> v == e) |> Dict.keys |> List.head


chooseMove : Grid.Point -> Grid.Point -> BigInt
chooseMove ( bx, _ ) ( px, _ ) =
    if bx > px then
        BigInt.fromInt 1

    else if px > bx then
        BigInt.fromInt -1

    else
        BigInt.fromInt 0


type StateB
    = InProgressB Intcode.Vm (Grid Entity) Int (List Int)
    | ErrorB (Grid Entity) String
    | AnswerB (Grid Entity) Int


initB : String -> StateB
initB =
    parseInput >> Intcode.setAbsInt 0 2 >> Intcode.vmFromTape >> (\vm -> InProgressB vm Dict.empty 0 [])


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB vm screen score buf ->
            case vm of
                Intcode.Halted _ ->
                    ( AnswerB screen score, True )

                Intcode.Running _ ->
                    case Intcode.intcodeStep vm of
                        Ok next ->
                            ( InProgressB next screen score buf, False )

                        Err txt ->
                            ( ErrorB screen txt, True )

                Intcode.WaitingToOutput _ val ->
                    let
                        newBuf =
                            Intcode.downcastUnsafe val :: buf
                    in
                    case newBuf of
                        v :: y :: x :: rest ->
                            if x == -1 && y == 0 then
                                ( InProgressB (Intcode.takeOutput vm) screen v [], False )

                            else
                                ( InProgressB (Intcode.takeOutput vm) (updateScreen x y (toEntity v) screen) score [], False )

                        _ ->
                            ( InProgressB (Intcode.takeOutput vm) screen score newBuf, False )

                Intcode.WaitingForInput _ _ ->
                    case ( findEntity Ball screen, findEntity Paddle screen ) of
                        ( Just ball, Just paddle ) ->
                            ( InProgressB (Intcode.giveInput (chooseMove ball paddle) vm) screen score buf, False )

                        _ ->
                            ( ErrorB screen "Didn't find ball or paddle", True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day13.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update 1000 1000



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 13
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.section "Part A"
            [ text "No magic here.  Just running the VM and interpreting the outputs as instructed.  Then count up the number of blocks in the screen grid."
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "At first I tried to play the game myself, but quickly realised I would need a (very simple) AI to play for me.  Here you can watch it play!"
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day13.elm"
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA ans ->
                    div [] [ text ("The answer is: " ++ String.fromInt ans) ]

                ErrorA txt ->
                    div [] [ text ("Error: " ++ txt) ]

                _ ->
                    div [] []

        _ ->
            div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Running state _ ->
            case state of
                InProgressB vm screen score _ ->
                    div []
                        [ viewGame screen
                        , text ("Score: " ++ String.fromInt score)
                        ]

                AnswerB screen ans ->
                    div []
                        [ viewGame screen
                        , text ("The answer is: " ++ String.fromInt ans)
                        ]

                ErrorB screen txt ->
                    div []
                        [ viewGame screen
                        , text ("Error: " ++ txt)
                        ]

        Exec.Paused state _ ->
            case state of
                InProgressB vm screen score _ ->
                    div []
                        [ viewGame screen
                        , text ("Score: " ++ String.fromInt score)
                        ]

                AnswerB screen ans ->
                    div []
                        [ viewGame screen
                        , text ("The answer is: " ++ String.fromInt ans)
                        ]

                ErrorB screen txt ->
                    div []
                        [ viewGame screen
                        , text ("Error: " ++ txt)
                        ]

        Exec.Finished state ->
            case state of
                AnswerB screen ans ->
                    div []
                        [ viewGame screen
                        , text ("Final score: " ++ String.fromInt ans)
                        ]

                ErrorB screen txt ->
                    div []
                        [ viewGame screen
                        , text ("Error: " ++ txt)
                        ]

                _ ->
                    div [] []

        _ ->
            div [] []


viewGame : Grid Entity -> Html Msg
viewGame =
    let
        drawObj =
            viewEntity
                >> List.singleton
                >> div
                    [ class "flex justify-center items-center"
                    , style "width" "16px"
                    , style "height" "16px"
                    ]

        packRow =
            div [ class "flex flex-row" ]

        packOutput =
            div [ class "flex flex-column" ]
    in
    Grid.draw drawObj packRow packOutput


viewEntity : Maybe Entity -> Html Msg
viewEntity e =
    case e of
        Just Wall ->
            Grid.drawHtmlCell "moon-gray" ""

        Just Block ->
            Grid.drawHtmlCell "blue" ""

        Just Paddle ->
            Grid.drawHtmlCell "yellow" ""

        Just Ball ->
            Grid.drawHtmlCell "red br-100" ""

        Nothing ->
            div [] []
