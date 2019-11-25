module Components exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)


title : String -> Html msg
title txt = div [ class "f2 tc mb3" ] [ text txt ]

codeBlock : String -> Html msg
codeBlock code = div [ class "f5 code bg-near-white pa1 ma2" ] [ text code ]

section : String -> List (Html msg) -> Html msg
section name children
    = div [ class "mv4 br3 ba b--black-10" ]
    [ div [ class "f4 bg-near-white br3 br--top pa2" ] [ text name ]
    , div [ class "bt b--black-10 pa2" ] children
    ]

problemInput : String -> String -> (String -> msg) -> Html msg
problemInput txt val action
    = div [ class "flex" ] [ input [ class "w-100 h2 ma2", placeholder txt, value val, onInput action ] [] ]
