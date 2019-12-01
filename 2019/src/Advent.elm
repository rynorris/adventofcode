module Advent exposing (..)

import Exec


type alias Model a b =
    { input : String
    , processA : Exec.Process a
    , processB : Exec.Process b
    }


type Msg a b
    = SetInput String
    | ControlA (Exec.Action a)
    | ControlB (Exec.Action b)


init : Model a b
init =
    { input = ""
    , processA = Exec.NotRunning
    , processB = Exec.NotRunning
    }


update : Msg a b -> Model a b -> ( Model a b, Cmd (Msg a b) )
update msg model =
    case msg of
        SetInput ans ->
            ( { model | input = ans }, Cmd.none )

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
