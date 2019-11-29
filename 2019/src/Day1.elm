module Day1 exposing (Model, Msg, init, name, update, view)

import Components as C
import Exec
import Grid
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)


name =
    "Day 1"



-- Solve Part A


type alias StateA =
    Int


initA : StateA
initA =
    0


stepA : Exec.StepFunction StateA
stepA state =
    if state > 1000000 then
        ( state, True )

    else
        ( state + 1, False )


batchSizeA =
    10000



-- UI


type alias Model =
    { input : String
    , processA : Exec.Process StateA
    }


init : Model
init =
    { input = "", processA = Exec.NotRunning }


type Msg
    = SetInput String
    | ControlA (Exec.Action StateA)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInput ans ->
            ( { model | input = ans }, Cmd.none )

        ControlA action ->
            let
                ( nextA, cmd ) =
                    Exec.control model.processA action (ControlA (Exec.Step batchSizeA))
            in
            ( { model | processA = nextA }, cmd )


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , text "Some notes on my solution to this problem."
        , C.section "Part A"
            [ text "The solution to part A"
            , C.codeBlock "a_code_snippet()"
            , C.problemInput "Enter problem input here" model.input SetInput
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA ControlA initA stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "The solution to part B"
            , C.link "Click here!" "https://somewhere.com"
            , text "Some more text"
            , Grid.drawHtml testDraw Grid.sample
            , C.largeProblemInput "Enter large input here" model.input SetInput
            ]
        ]


testDraw : Maybe Int -> Html Msg
testDraw x =
    case x of
        Just 1 ->
            Grid.drawHtmlCell "red" "1"

        Just 2 ->
            Grid.drawHtmlCell "blue" "2"

        Just 3 ->
            Grid.drawHtmlCell "orange" "3"

        Just _ ->
            Grid.drawHtmlCell "green" "4"

        Nothing ->
            div [] []


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.NotRunning ->
            div [] []

        Exec.Running val _ ->
            C.progressBar val 1000000

        Exec.Paused val _ ->
            C.progressBar val 1000000

        Exec.Finished val ->
            div [ class "f3" ] [ text ("Answer: " ++ String.fromInt val) ]
