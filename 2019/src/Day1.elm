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
                ( nextA, running ) =
                    Exec.control model.processA action
            in
            ( { model | processA = nextA }
            , if running then
                Exec.delay (ControlA (Exec.Step batchSizeA))

              else
                Cmd.none
            )


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
                [ viewButtonA model
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
            div [ class "bg-red" ] []

        Just 2 ->
            div [ class "bg-blue" ] []

        Just 3 ->
            div [ class "bg-yellow" ] []

        Just _ ->
            div [ class "bg-green" ] []

        Nothing ->
            div [] []


viewButtonA : Model -> Html Msg
viewButtonA model =
    case model.processA of
        Exec.NotRunning ->
            C.runButton "Run" (ControlA (Exec.Start initA stepA))

        Exec.Running _ _ ->
            C.runButton "Pause" (ControlA Exec.Pause)

        Exec.Paused _ _ ->
            C.runButton "Continue" (ControlA Exec.Continue)

        Exec.Finished val ->
            C.runButton "Reset" (ControlA Exec.Reset)


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
