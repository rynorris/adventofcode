module Components exposing (..)

import Advent
import Exec
import Html exposing (Html, code, div, input, pre, progress, text)
import Html.Attributes as A exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)


title : String -> Html msg
title txt =
    div [ class "f2 tc mb3" ] [ text txt ]


codeBlock : String -> Html msg
codeBlock src =
    div [ class "h5 mh2 overflow-auto" ]
        [ useTheme monokai
        , elm src
            |> Result.map (toBlockHtml (Just 1))
            |> Result.withDefault
                (pre [] [ code [] [ text src ] ])
        ]


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


loadExampleButtons : List Advent.ExampleInput -> Html (Advent.Msg a b)
loadExampleButtons examples =
    examples
        |> List.map (\e -> actionButton e.name (Advent.SetInput e.input))
        |> div [ class "flex flex-wrap justify-center" ]


progressBar : Int -> Int -> Html msg
progressBar value max =
    progress [ class "w-100 mv2 bg-dark-green", A.max (String.fromInt max), A.value (String.fromInt value) ] []


controlProcessButton : Exec.Process a -> (Exec.Action a -> msg) -> a -> Exec.StepFunction a -> Html msg
controlProcessButton process control init step =
    case process of
        Exec.NotRunning ->
            actionButton "Run" (control (Exec.Start init step))

        Exec.Running _ _ ->
            actionButton "Pause" (control Exec.Pause)

        Exec.Paused _ _ ->
            actionButton "Continue" (control Exec.Continue)

        Exec.Finished val ->
            actionButton "Reset" (control Exec.Reset)


actionButton : String -> msg -> Html msg
actionButton txt m =
    div [ class "f4 link br3 ba bw1 ma2 ph4 pv2 dib moon-gray hover-dark-green pointer", onClick m ] [ text txt ]


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


partASource : Advent.Model a b -> Html (Advent.Msg a b)
partASource =
    sourceSegment .partA


partBSource : Advent.Model a b -> Html (Advent.Msg a b)
partBSource =
    sourceSegment .partB


sourceSegment : (Advent.ProblemSource -> String) -> Advent.Model a b -> Html (Advent.Msg a b)
sourceSegment segment model =
    model.source |> Maybe.map segment |> Maybe.map codeBlock |> Maybe.withDefault (Html.div [] [])
