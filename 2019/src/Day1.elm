module Day1 exposing (Model, Msg, init, name, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick)

import Components as C
import Exec


name = "Day 1"


-- Define Process A
type alias StateA = Int


initA : StateA
initA = 0


stepA : StateA -> Exec.Process StateA
stepA state = if state > 1000000 then Exec.Finished state else Exec.Running (state + 1)


batchA = Exec.batch 10000 stepA


-- Define Model
type alias Model =
    { input : String
    , processA : Exec.Process StateA
    }


init : Model
init = 
    { input = "", processA = Exec.NotRunning }


type Msg
    = SetInput String
    | StartA
    | StepA (Exec.Process StateA)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetInput ans -> ({ model | input = ans }, Cmd.none)
        StartA -> (model, Exec.continue (Exec.Running initA) batchA StepA)
        StepA process -> ({ model | processA = process }, Exec.continue process batchA StepA)


view : Model -> Html Msg
view model =
    div []
    [ C.title name
    , text "Some notes on my solution to this problem."
    , C.problemInput "Enter input" model.input SetInput
    , button [ onClick StartA ] [ text "Start!" ]
    , viewAnswerA model
    , C.section "Part A"
        [ text "The solution to part A"
        , C.codeBlock "a_code_snippet()"
        ]
    , C.section "Part B"
        [ text "The solution to part B"
        , C.problemInput "Input for part B" model.input SetInput
        ]
    ]


viewAnswerA : Model -> Html Msg
viewAnswerA model =
    case model.processA of
        Exec.NotRunning -> div [] []
        Exec.Running val -> text ("Running... " ++ String.fromInt(val))
        Exec.Finished val -> text ("Finished: " ++ String.fromInt(val))
