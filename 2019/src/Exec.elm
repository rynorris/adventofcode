module Exec exposing (Action(..), Process(..), StepFunction, batch, control, delay)

import Process as P
import Task
import Time


type Process s
    = NotRunning
    | Running s (StepFunction s)
    | Paused s (StepFunction s)
    | Finished s


type Action s
    = Start s (StepFunction s)
    | Step Int
    | Pause
    | Continue
    | Reset


type alias StepFunction s =
    s -> ( s, Bool )



-- Process State Machine Control


control : Process s -> Action s -> msg -> ( Process s, Cmd msg )
control process action continue =
    case ( process, action ) of
        ( NotRunning, Start state fun ) ->
            ( Running state fun, delay continue )

        ( Running state fun, Step num ) ->
            let
                next =
                    batch num process
            in
            ( next
            , if isRunning next then
                delay continue

              else
                Cmd.none
            )

        ( Running state fun, Pause ) ->
            ( Paused state fun, Cmd.none )

        ( Paused state fun, Continue ) ->
            ( Running state fun, delay continue )

        ( Finished state, Reset ) ->
            ( NotRunning, Cmd.none )

        _ ->
            ( process, Cmd.none )


isRunning : Process s -> Bool
isRunning process =
    case process of
        Running _ _ ->
            True

        _ ->
            False


step : Process s -> Process s
step process =
    case process of
        Running state fun ->
            let
                ( next, finished ) =
                    fun state
            in
            if finished then
                Finished next

            else
                Running next fun

        _ ->
            process


batch : Int -> Process s -> Process s
batch num process =
    if num == 0 then
        process

    else
        let
            next =
                step process
        in
        case next of
            Running _ _ ->
                batch (num - 1) next

            _ ->
                next


delay : msg -> Cmd msg
delay bldMsg =
    Task.perform (\_ -> bldMsg) (P.sleep 10 |> Task.andThen (\_ -> Task.succeed ()))
