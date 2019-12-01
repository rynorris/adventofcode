module Advent exposing (..)

import Components as C
import Exec
import Html exposing (Html)
import Http
import Parser exposing ((|.), (|=), Parser, chompUntil, getChompedString, succeed)


type alias Model a b =
    { input : String
    , source : Maybe ProblemSource
    , processA : Exec.Process a
    , processB : Exec.Process b
    }


type alias ProblemSource =
    { common : String
    , partA : String
    , partB : String
    }


type Msg a b
    = SetInput String
    | SetSource (Maybe ProblemSource)
    | ControlA (Exec.Action a)
    | ControlB (Exec.Action b)


init : Model a b
init =
    { input = ""
    , source = Nothing
    , processA = Exec.NotRunning
    , processB = Exec.NotRunning
    }


update : Msg a b -> Model a b -> ( Model a b, Cmd (Msg a b) )
update msg model =
    case msg of
        SetInput ans ->
            ( { model | input = ans }, Cmd.none )

        SetSource src ->
            ( { model | source = src }, Cmd.none )

        ControlA action ->
            let
                ( nextA, cmd ) =
                    Exec.control model.processA action (ControlA (Exec.Step 1000))
            in
            ( { model | processA = nextA }, cmd )

        ControlB action ->
            let
                ( nextB, cmd ) =
                    Exec.control model.processB action (ControlB (Exec.Step 1000))
            in
            ( { model | processB = nextB }, cmd )


viewPartASource : Model a b -> Html (Msg a b)
viewPartASource =
    viewSourceSegment .partA


viewPartBSource : Model a b -> Html (Msg a b)
viewPartBSource =
    viewSourceSegment .partB


viewSourceSegment : (ProblemSource -> String) -> Model a b -> Html (Msg a b)
viewSourceSegment segment model =
    model.source |> Maybe.map segment |> Maybe.map C.codeBlock |> Maybe.withDefault (Html.div [] [])


loadSourceFile : String -> Cmd (Msg a b)
loadSourceFile filename =
    Http.get
        { url = "https://raw.githubusercontent.com/rynorris/adventofcode/master/2019/src/" ++ filename
        , expect = Http.expectString (Result.withDefault "" >> Parser.run sourceFileParser >> Result.toMaybe >> SetSource)
        }


sourceFileParser : Parser ProblemSource
sourceFileParser =
    succeed ProblemSource
        |. chompUntil "-- Common"
        |= readTextUntil "-- Solve Part A"
        |= readTextUntil "-- Solve Part B"
        |= readTextUntil "-- Model"


readTextUntil : String -> Parser String
readTextUntil s =
    getChompedString <|
        succeed ()
            |. chompUntil s
