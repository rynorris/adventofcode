module Day4 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Exec
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Parser exposing ((|.), (|=), Parser, succeed)
import Set exposing (Set)


name =
    "Day 4: Secure Container"



-- Common


inputParser : Parser ( Int, Int )
inputParser =
    succeed (\x y -> ( x, y ))
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int


parseInput : String -> ( Int, Int )
parseInput =
    Parser.run inputParser >> Result.toMaybe >> Maybe.withDefault ( 0, 0 )



-- Solve Part A


decreaseIndex : List comparable -> Maybe Int
decreaseIndex xs =
    List.map2 (\a b -> a > b) xs (List.drop 1 xs)
        |> List.indexedMap (\ix isDec -> ( ix, isDec ))
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.head


nextCandidate : Int -> Int
nextCandidate x =
    let
        l =
            x |> String.fromInt |> String.toList
    in
    case decreaseIndex l of
        Just ix ->
            let
                multiplier =
                    10 ^ (List.length l - ix - 2)
            in
            nextCandidate ((x // multiplier + 1) * multiplier)

        Nothing ->
            x


splitRuns : List comparable -> List (List comparable)
splitRuns xs =
    let
        processOne x runs =
            case runs of
                [] ->
                    [ [ x ] ]

                [] :: rs ->
                    [ x ] :: rs

                (y :: ys) :: rs ->
                    if x == y then
                        (x :: y :: ys) :: rs

                    else
                        [ x ] :: (y :: ys) :: rs
    in
    List.foldl processOne [] xs


runLengths : List comparable -> List Int
runLengths =
    splitRuns >> List.map List.length


hasADouble : List comparable -> Bool
hasADouble =
    runLengths >> List.any (\x -> x >= 2)


isValidA : Int -> Bool
isValidA =
    String.fromInt >> String.toList >> hasADouble


type StateA
    = InProgressA Int Int Int Int
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> (\( min, max ) -> InProgressA min max min 0)


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA min max cur count ->
            if cur > max then
                ( AnswerA count, True )

            else if isValidA cur then
                ( InProgressA min max (nextCandidate (cur + 1)) (count + 1), False )

            else
                ( InProgressA min max (nextCandidate (cur + 1)) count, False )

        _ ->
            ( state, True )



-- Solve Part B


hasAnExactDouble : List comparable -> Bool
hasAnExactDouble =
    runLengths >> List.any (\x -> x == 2)


isValidB : Int -> Bool
isValidB =
    String.fromInt >> String.toList >> hasAnExactDouble


type StateB
    = InProgressB Int Int Int Int
    | AnswerB Int


initB : String -> StateB
initB =
    parseInput >> (\( min, max ) -> InProgressB min max min 0)


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB min max cur count ->
            if cur > max then
                ( AnswerB count, True )

            else if isValidB cur then
                ( InProgressB min max (nextCandidate (cur + 1)) (count + 1), False )

            else
                ( InProgressB min max (nextCandidate (cur + 1)) count, False )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day4.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update 1000 1000



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 4
        , Html.p []
            [ text "This problem was slightly disappointing for me.  There's lots of scope for clever optimizations here, but the input ranges are small enough that brute-force is feasible on modern machines, so there's no incentive to do so." ]
        , Html.p []
            [ text "However when I ran this page on a mobile device, the brute-force approach was very slow.  So I made an optimization such that whenever we detect a number with decreasing digits, we skip ahead to the next number without a decrease.  e.g. from 123000 -> 123300" ]
        , C.problemInput "Enter input here (e.g. \"1234-5678\")" model.input Advent.SetInput
        , C.section "Part A"
            [ text "For this problem I just brute force check every value between the bounds with the above noted optimization."
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "Same as part A just with a slightly modified condition."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day1.elm"
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA ans ->
                    div [ class "f3" ] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        Exec.Running state _ ->
            case state of
                InProgressA min max cur count ->
                    viewInProgressA min max cur count

                _ ->
                    div [] []

        Exec.Paused state _ ->
            case state of
                InProgressA min max cur count ->
                    viewInProgressA min max cur count

                _ ->
                    div [] []

        _ ->
            div [] []


viewInProgressA : Int -> Int -> Int -> Int -> Html Msg
viewInProgressA min max cur count =
    div [ class "f3 w-100" ]
        [ Html.p [] [ text ("Searching between " ++ String.fromInt min ++ " and " ++ String.fromInt max) ]
        , Html.p [] [ text ("Currently looking at: " ++ String.fromInt cur ++ ", Found so far: " ++ String.fromInt count) ]
        , C.progressBar (cur - min) (max - min)
        ]


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Finished state ->
            case state of
                AnswerB ans ->
                    div [ class "f3" ] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        Exec.Running state _ ->
            case state of
                InProgressB min max cur count ->
                    viewInProgressB min max cur count

                _ ->
                    div [] []

        Exec.Paused state _ ->
            case state of
                InProgressB min max cur count ->
                    viewInProgressB min max cur count

                _ ->
                    div [] []

        _ ->
            div [] []


viewInProgressB : Int -> Int -> Int -> Int -> Html Msg
viewInProgressB min max cur count =
    div [ class "f3 w-100" ]
        [ Html.p [] [ text ("Searching between " ++ String.fromInt min ++ " and " ++ String.fromInt max) ]
        , Html.p [] [ text ("Currently looking at: " ++ String.fromInt cur ++ ", Found so far: " ++ String.fromInt count) ]
        , C.progressBar (cur - min) (max - min)
        ]
