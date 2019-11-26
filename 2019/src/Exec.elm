module Exec exposing (Action(..), Process(..), batch, control, delay)

import Process as P
import Task
import Time

type Process s
    = NotRunning
    | Running s
    | Paused s
    | Finished s


type Action s
    = Start s
    | Step (s -> Process s)
    | Pause
    | Continue
    | Reset


control : Process s -> Action s -> (Process s, Bool)
control process action =
    case action of
        Start state -> (start process state, True)
        Step fun -> 
            let
                next = step process fun
            in
                (next, isRunning next)
        Pause -> (pause process, False)
        Continue -> (continue process, True)
        Reset -> (NotRunning, False)


isRunning : Process s -> Bool
isRunning process =
    case process of
        Running _ -> True
        _ -> False


step : Process s -> (s -> Process s) -> Process s
step process fun = 
    case process of
        Running state -> fun state
        _ -> process


start : Process s -> s -> Process s
start process init =
    case process of
        NotRunning -> Running init
        _ -> process


continue : Process s -> Process s
continue process =
    case process of
        Paused state -> Running state
        _ -> process


pause : Process s -> Process s
pause process =
    case process of
        Running state -> Paused state
        _ -> process


delay : msg -> Cmd msg
delay bldMsg = Task.perform (\_ -> bldMsg) (P.sleep 10 |> Task.andThen (\_ -> Task.succeed ()))


batch : Int -> (s -> Process s) -> s -> Process s
batch num fun state =
    if num == 1 then
        fun state
    else 
        let next = fun state in
            case next of
                NotRunning -> NotRunning
                Running newState -> batch (num - 1) fun newState
                Paused s -> Paused s
                Finished finalState -> Finished finalState
