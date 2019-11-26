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


control : Process s -> Action s -> ( Process s, Bool )
control process action =
    case action of
        Start state fun ->
            ( start process state fun, True )

        Step num ->
            let
                next =
                    batch num process
            in
            ( next, isRunning next )

        Pause ->
            ( pause process, False )

        Continue ->
            ( continue process, True )

        Reset ->
            ( NotRunning, False )


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


start : Process s -> s -> StepFunction s -> Process s
start process init fun =
    case process of
        NotRunning ->
            Running init fun

        _ ->
            process


continue : Process s -> Process s
continue process =
    case process of
        Paused state fun ->
            Running state fun

        _ ->
            process


pause : Process s -> Process s
pause process =
    case process of
        Running state fun ->
            Paused state fun

        _ ->
            process


delay : msg -> Cmd msg
delay bldMsg =
    Task.perform (\_ -> bldMsg) (P.sleep 10 |> Task.andThen (\_ -> Task.succeed ()))


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
