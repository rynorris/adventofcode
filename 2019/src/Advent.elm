module Advent exposing (..)

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


type alias ExampleInput =
    { name : String
    , input : String
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


update : Int -> Int -> Msg a b -> Model a b -> ( Model a b, Cmd (Msg a b) )
update batchA batchB msg model =
    case msg of
        SetInput ans ->
            ( { model | input = ans }, Cmd.none )

        SetSource src ->
            ( { model | source = src }, Cmd.none )

        ControlA action ->
            let
                ( nextA, cmd ) =
                    Exec.control model.processA action (ControlA (Exec.Step batchA))
            in
            ( { model | processA = nextA }, cmd )

        ControlB action ->
            let
                ( nextB, cmd ) =
                    Exec.control model.processB action (ControlB (Exec.Step batchB))
            in
            ( { model | processB = nextB }, cmd )


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
