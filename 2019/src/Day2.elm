module Day2 exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, text)


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
    div [] [ text "CHALLENGE 2" ]
