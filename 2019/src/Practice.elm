module Practice exposing (Model, Msg, init, name, update, view)

import Components as C
import Exec
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Practice (2018 Day 1)"



-- Common


filterOut : String -> String -> String
filterOut chars =
    String.filter (\c -> String.toList chars |> List.member c |> not)


parseInput : String -> List Int
parseInput =
    filterOut "," >> String.words >> List.map (String.toInt >> Maybe.withDefault 0)



-- Solve Part A


type StateA
    = InputA (List Int)
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> InputA


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InputA xs ->
            ( AnswerA (List.sum xs), True )

        AnswerA x ->
            ( AnswerA x, True )



-- Solve Part B


type StateB
    = InProgressB { input : List Int, curr : Int, iter : List Int, seen : Set Int }
    | AnswerB Int


initB : String -> StateB
initB s =
    InProgressB { input = parseInput s, curr = 0, iter = [], seen = Set.empty }


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB prog ->
            if Set.member prog.curr prog.seen then
                ( AnswerB prog.curr, True )

            else
                case prog.iter of
                    [] ->
                        ( InProgressB { prog | iter = prog.input }, False )

                    x :: xs ->
                        ( InProgressB { prog | curr = prog.curr + x, iter = xs, seen = Set.insert prog.curr prog.seen }, False )

        _ ->
            ( state, True )



-- UI and State


type alias Model =
    { input : String
    , processA : Exec.Process StateA
    , processB : Exec.Process StateB
    }


init : Model
init =
    { input = ""
    , processA = Exec.NotRunning
    , processB = Exec.NotRunning
    }


type Msg
    = SetInput String
    | ControlA (Exec.Action StateA)
    | ControlB (Exec.Action StateB)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInput ans ->
            ( { model | input = ans }, Cmd.none )

        ControlA action ->
            let
                ( nextA, cmd ) =
                    Exec.control model.processA action (ControlA (Exec.Step 1))
            in
            ( { model | processA = nextA }, cmd )

        ControlB action ->
            let
                ( nextB, cmd ) =
                    Exec.control model.processB action (ControlB (Exec.Step 1000))
            in
            ( { model | processB = nextB }, cmd )


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , text "As a practice test of my elm infrastructure, I am re-solving Day 1 from 2018s AoC"
        , C.adventOfCodeProblemLink 2018 1
        , C.largeProblemInput "Paste input here" model.input SetInput
        , C.section "Part A"
            [ text "Part A is just computing a simple sum of the input integers."
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "For part B we continue iterating through the input, keeping a Set of all the frequencies we've seen until we find a duplicate."
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA x ->
                    div [ class "f3" ] [ text ("Answer: " ++ String.fromInt x) ]

                _ ->
                    div [ class "f3" ] [ text "Invalid state!  Finished processing without reaching answer." ]

        _ ->
            div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Finished state ->
            case state of
                AnswerB x ->
                    div [ class "f3" ] [ text ("Answer: " ++ String.fromInt x) ]

                _ ->
                    div [ class "f3" ] [ text "Invalid state!  Finished processing without reaching answer." ]

        Exec.Running state _ ->
            case state of
                InProgressB prog ->
                    div [ class "f3" ] [ text ("Seen " ++ String.fromInt (Set.size prog.seen) ++ " unique frequencies.") ]

                _ ->
                    div [ class "f3" ] [ text "Invalid state!  Reached answer without finishing." ]

        Exec.Paused state _ ->
            case state of
                InProgressB prog ->
                    div [ class "f3" ] [ text ("Seen " ++ String.fromInt (Set.size prog.seen) ++ " unique frequencies. (Paused)") ]

                _ ->
                    div [ class "f3" ] [ text "Invalid state!  Reached answer without finishing." ]

        _ ->
            div [] []
