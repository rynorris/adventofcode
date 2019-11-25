module Components exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


title : String -> Html msg
title txt = div [ class "f1 tc" ] [ text txt ]

section : String -> List (Html msg) -> Html msg
section name children
    = div [ class "mv4 br3 ba b--black-10" ]
    [ div [ class "f4 bg-near-white br3 br--top pa2" ] [ text name ]
    , div [ class "bt b--black-10 pa2" ] children
    ]

