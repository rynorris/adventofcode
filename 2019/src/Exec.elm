module Exec exposing (Process(..), batch, continue, start)

import Process as P
import Task
import Time

type Process s
    = NotRunning
    | Running s
    | Finished s


step : Process s -> (s -> Process s) -> Process s
step process fun = 
    case process of
        NotRunning -> NotRunning
        Running state -> fun state
        Finished state -> Finished state


start : Process s -> s -> (s -> Process s) -> (Process s -> msg) -> Cmd msg
start process init fun unwrap =
    case process of
        NotRunning -> Task.perform unwrap (P.sleep 10 |> Task.andThen (\_ -> Task.succeed (step (Running init) fun)))
        Running _ -> Cmd.none
        Finished _ -> Task.perform unwrap (P.sleep 10 |> Task.andThen (\_ -> Task.succeed (step (Running init) fun)))


continue : Process s -> (s -> Process s) -> (Process s -> msg) -> Cmd msg
continue process fun unwrap =
    case process of
        NotRunning -> Cmd.none
        Running _ -> Task.perform unwrap (P.sleep 10 |> Task.andThen (\_ -> Task.succeed (step process fun)))
        Finished _ -> Cmd.none


batch : Int -> (s -> Process s) -> s -> Process s
batch num fun state =
    if num == 1 then
        fun state
    else 
        let next = fun state in
            case next of
                NotRunning -> NotRunning
                Running newState -> batch (num - 1) fun newState
                Finished finalState -> Finished finalState
