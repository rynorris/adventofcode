module Day1 exposing (Model, Msg, init, name, update, view)

import Html exposing (Html, div, text)

import Components as C


name = "Day 1"


type alias Model =
    { answer : String
    }


init : Model
init = 
    { answer = "Something" }


type Msg
    = SetAnswer String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetAnswer ans ->
            { model | answer = ans }


view : Model -> Html Msg
view model =
    div []
    [ C.title name
    , C.section "Part A" [ text "The solution to part A" ]
    , C.section "Part B" [ text "The solution to part B" ]
    ]
