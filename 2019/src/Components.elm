module Components exposing (..)

import Html exposing (Html, div, input, progress, text)
import Html.Attributes as A exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)


title : String -> Html msg
title txt =
    div [ class "f2 tc mb3" ] [ text txt ]


codeBlock : String -> Html msg
codeBlock code =
    div [ class "f5 code bg-moon-gray dark-green pa1 ma2" ] [ text code ]


section : String -> List (Html msg) -> Html msg
section name children =
    div [ class "mv4 br3 ba b--dark-green" ]
        [ div [ class "f4 bg-dark-green moon-gray br3 br--top pa2" ] [ text name ]
        , div [ class "bt b--dark-green pa2" ] children
        ]


problemInput : String -> String -> (String -> msg) -> Html msg
problemInput txt val action =
    div [ class "flex" ] [ input [ class "w-100 h2 ma2", placeholder txt, A.value val, onInput action ] [] ]


progressBar : Int -> Int -> Html msg
progressBar value max =
    progress [ class "w-100 mv2 bg-dark-green", A.max (String.fromInt max), A.value (String.fromInt value) ] []


runButton : String -> msg -> Html msg
runButton txt m =
    div [ class "f4 link br3 ba bw1 mv2 ph4 pv2 dib moon-gray hover-dark-green pointer", onClick m ] [ text txt ]


link : String -> String -> Html msg
link txt url =
    Html.a [ class "link dark-green mh1", A.href url ] [ text txt ]
