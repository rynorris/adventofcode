module Day8 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Dict
import Exec
import Grid exposing (Grid, Point)
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Day 8: Space Image Format"



-- Solve Part A


width =
    25


height =
    6


countDigits : Char -> String -> Int
countDigits c =
    String.filter (\x -> x == c) >> String.length


splitLayers : String -> List String
splitLayers s =
    case s of
        "" ->
            []

        cs ->
            String.left (width * height) s :: splitLayers (String.dropLeft (width * height) s)


solveA : String -> Int
solveA s =
    let
        layers =
            splitLayers s
    in
    layers |> List.sortBy (countDigits '0') |> List.head |> Maybe.withDefault "" |> (\l -> countDigits '1' l * countDigits '2' l)


type StateA
    = InProgressA String
    | AnswerA Int


initA : String -> StateA
initA =
    InProgressA


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA img ->
            ( AnswerA (solveA img), True )

        _ ->
            ( state, True )



-- Solve Part B


type Pixel
    = Transparent
    | White
    | Black


type StateB
    = InProgressB String
    | AnswerB (Grid Pixel)


initB : String -> StateB
initB =
    InProgressB


parseImage : String -> List Pixel
parseImage =
    String.toList >> List.map parsePixel


parsePixel : Char -> Pixel
parsePixel c =
    case c of
        '0' ->
            White

        '1' ->
            Black

        _ ->
            Transparent


renderImage : Int -> Int -> List Pixel -> Grid Pixel
renderImage w h =
    let
        applyPixel ( loc, px ) grd =
            case Dict.get loc grd of
                Just Transparent ->
                    Dict.insert loc px grd

                Nothing ->
                    Dict.insert loc px grd

                _ ->
                    grd
    in
    List.indexedMap (\ix px -> ( ( modBy w ix, modBy (w * h) ix // w ), px )) >> List.foldl applyPixel Dict.empty


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB img ->
            ( AnswerB (parseImage img |> renderImage width height), True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day8.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update 1000 1000



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 8
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.section "Part A"
            [ text "I just solve this part naively by splitting the input into layers and counting occurances of each value per layer."
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "Rendering the image is done using a fold over the input pixels, updating the final grid of pixels as we go."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day8.elm"
        ]


viewProgressA : Model -> Html Msg
viewProgressA model =
    case model.processA of
        Exec.Finished state ->
            case state of
                AnswerA ans ->
                    div [] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        _ ->
            div [] []


viewProgressB : Model -> Html Msg
viewProgressB model =
    case model.processB of
        Exec.Finished state ->
            case state of
                AnswerB img ->
                    Grid.drawHtml "15px" viewPixel img

                _ ->
                    div [] []

        _ ->
            div [] []


viewPixel : Maybe Pixel -> Html Msg
viewPixel p =
    case p of
        Just Black ->
            div [ class "w-100 h-100 bg-black" ] []

        Just White ->
            div [ class "w-100 h-100 bg-white" ] []

        _ ->
            div [] []
