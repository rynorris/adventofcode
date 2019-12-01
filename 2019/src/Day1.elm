module Day1 exposing (Model, Msg, init, name, update, view)

import Advent
import Components as C
import Exec
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Day 1"



-- Solve Part A


type StateA
    = InProgressA
    | AnswerA


initA : String -> StateA
initA s =
    InProgressA


stepA : Exec.StepFunction StateA
stepA state =
    ( AnswerA, True )



-- Solve Part B


type StateB
    = InProgressB
    | AnswerB


initB : String -> StateB
initB s =
    InProgressB


stepB : Exec.StepFunction StateB
stepB state =
    ( AnswerB, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , text "The solution to this problem will go live 24 hours after the problem closes."
        , C.section "Part A"
            [ text "Coming soon!"
            ]
        , C.section "Part B"
            [ text "Coming soon!"
            ]
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    div [] []
