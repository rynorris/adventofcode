module Home exposing (..)

import Components as C
import Html exposing (Html, button, div, text)


overviewBody =
    [ Html.p []
        [ C.link "Advent of Code" "https://adventofcode.com/"
        , text "is a yearly programming challenge taking place throughout December each year."
        ]
    , Html.p []
        [ text "One puzzle is released each day, with difficulty generally ramping up throughout the month." ]
    , Html.p []
        [ text """
While there is a competition element, many people (including myself) treat this as an opportunity
to learn something new, try a new programming language etc.
"""
        ]
    , Html.p []
        [ text "For 2019, I decided to write my solutions as an interactive web page using "
        , C.link "elm." "https://elm-lang.org/"
        ]
    ]


elmBody =
    [ Html.p []
        [ text "Running all the puzzle solutions in the front-end poses some interesting problems." ]
    , Html.p []
        [ text "Primarily the issue is that we don't want long-running computations to run synchronously, blocking page rendering." ]
    , Html.p []
        [ text "One possible solution to this would be to run the computation in a WebWorker.  However elm doesn't have good support for WebWorkers yet, so I ruled this out." ]
    , Html.p []
        [ text "In the end I have modeled each problem solution as a state and a step function." ]
    , Html.p []
        [ text "While the computation is still running, it will run N steps before sending an elm Task to sleep for 10ms before sending a Cmd to trigger another round of processing." ]
    , Html.p []
        [ text "This means I must fit all of my solutions this year into this state & step model, which may prove challenging." ]
    , Html.p []
        [ text "You can see the relevant code for this execution model in "
        , C.link "Exec.elm" "https://github.com/rynorris/adventofcode/blob/master/2019/src/Exec.elm"
        ]
    ]


view : Html msg
view =
    div []
        [ C.title "Advent of Code 2019"
        , C.section "Overview" overviewBody
        , C.section "Elm" elmBody
        ]
