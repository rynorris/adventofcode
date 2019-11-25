module Day1 exposing (Model, Msg, init, name, update, view)

import Html exposing (Html, div, input, text)

import Components as C


name = "Day 1"


type alias Model =
    { input : String
    , answer : String
    }


init : Model
init = 
    { input = "", answer = "" }


type Msg
    = SetInput String
    | SetAnswer String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput ans ->
            { model | input = ans }
        SetAnswer ans ->
            { model | answer = ans }


view : Model -> Html Msg
view model =
    div []
    [ C.title name
    , text "Some notes on my solution to this problem."
    , C.problemInput "Enter input" model.input SetInput
    , C.section "Part A"
        [ text "The solution to part A"
        , C.codeBlock "a_code_snippet()"
        ]
    , C.section "Part B"
        [ text "The solution to part B"
        , C.problemInput "Input for part B" model.input SetInput
        ]
    ]
