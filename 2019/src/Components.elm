module Components exposing (..)

import Exec
import Html exposing (Html, div, input, progress, text)
import Html.Attributes as A exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)


title : String -> Html msg
title txt =
    div [ class "f2 tc mb3" ] [ text txt ]


codeBlock : String -> Html msg
codeBlock code =
    Html.pre [ class "f5 h5 overflow-auto mh5 code bg-moon-gray dark-green pa1 ma2" ] [ text code ]


section : String -> List (Html msg) -> Html msg
section name children =
    div [ class "mv4 br3 ba bw1 b--dark-green" ]
        [ div [ class "f4 bg-dark-green moon-gray pa2" ] [ text name ]
        , div [ class "bt b--dark-green pa2" ] children
        ]


problemInput : String -> String -> (String -> msg) -> Html msg
problemInput txt val action =
    div [ class "flex" ] [ input [ class "w-100 h2 ma3", placeholder txt, A.value val, onInput action ] [] ]


largeProblemInput : String -> String -> (String -> msg) -> Html msg
largeProblemInput txt val action =
    div [ class "flex" ] [ Html.textarea [ class "w-100 h5 ma3", placeholder txt, A.value val, onInput action ] [] ]


progressBar : Int -> Int -> Html msg
progressBar value max =
    progress [ class "w-100 mv2 bg-dark-green", A.max (String.fromInt max), A.value (String.fromInt value) ] []


controlProcessButton : Exec.Process a -> (Exec.Action a -> msg) -> a -> Exec.StepFunction a -> Html msg
controlProcessButton process control init step =
    case process of
        Exec.NotRunning ->
            runButton "Run" (control (Exec.Start init step))

        Exec.Running _ _ ->
            runButton "Pause" (control Exec.Pause)

        Exec.Paused _ _ ->
            runButton "Continue" (control Exec.Continue)

        Exec.Finished val ->
            runButton "Reset" (control Exec.Reset)


runButton : String -> msg -> Html msg
runButton txt m =
    div [ class "f4 link br3 ba bw1 mv2 ph4 pv2 dib moon-gray hover-dark-green pointer", onClick m ] [ text txt ]


link : String -> String -> Html msg
link txt url =
    Html.a [ class "link dark-green mh1", A.href url, A.target "_blank" ] [ text txt ]


adventOfCodeProblemLink : Int -> Int -> Html msg
adventOfCodeProblemLink year day =
    div []
        [ link ("View " ++ String.fromInt year ++ " day " ++ String.fromInt day ++ " puzzle details here.") ("https://adventofcode.com/" ++ String.fromInt year ++ "/day/" ++ String.fromInt day)
        ]


sourceCodeLink : String -> Html msg
sourceCodeLink filename =
    div []
        [ link "View the source for this page on GitHub!" ("https://github.com/rynorris/adventofcode/blob/master/2019/src/" ++ filename)
        ]
