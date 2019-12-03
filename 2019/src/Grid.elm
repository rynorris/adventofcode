module Grid exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as A exposing (class)


type alias Point =
    ( Int, Int )


type alias Grid obj =
    Dict Point obj


sample : Grid Int
sample =
    Dict.fromList [ ( ( 0, 0 ), 1 ), ( ( 3, 2 ), 2 ), ( ( 2, 2 ), 3 ), ( ( 1, 3 ), 4 ) ]


manhattan : Point -> Point -> Int
manhattan ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


pointAdd : Point -> Point -> Point
pointAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


scalarMul : Int -> Point -> Point
scalarMul a ( x, y ) =
    ( a * x, a * y )


drawHtml : (Maybe obj -> Html msg) -> Grid obj -> Html msg
drawHtml drawOne =
    let
        drawObj =
            drawOne >> List.singleton >> div [ class "ba b--moon-gray h3 w3 flex justify-center items-center" ]

        packRow =
            div [ class "flex flex-row" ]

        packOutput =
            div [ class "flex flex-column" ]
    in
    draw drawObj packRow packOutput


drawHtmlCell : String -> String -> Html msg
drawHtmlCell colour txt =
    div [ class ("w-100 h-100 flex items-center justify-center bg-" ++ colour) ] [ text txt ]


draw : (Maybe obj -> element) -> (List element -> row) -> (List row -> out) -> Grid obj -> out
draw drawObj packRow packOutput grid =
    let
        minX =
            grid |> Dict.keys |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0

        maxX =
            grid |> Dict.keys |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0
    in
    grid
        |> getRows
        |> List.map (drawRow drawObj minX maxX >> packRow)
        |> packOutput


drawRow : (Maybe obj -> element) -> Int -> Int -> List ( Point, obj ) -> List element
drawRow drawObj minX maxX =
    expandRow minX maxX >> List.map drawObj


expandRow : Int -> Int -> List ( Point, obj ) -> List (Maybe obj)
expandRow minX maxX items =
    let
        item =
            List.head items
    in
    case item of
        Nothing ->
            if minX <= maxX then
                Nothing :: expandRow (minX + 1) maxX []

            else
                []

        Just ( ( x, _ ), obj ) ->
            if x == maxX then
                [ Just obj ]

            else if x == minX then
                Just obj :: expandRow (minX + 1) maxX (List.tail items |> Maybe.withDefault [])

            else if x > minX then
                Nothing :: expandRow (minX + 1) maxX items

            else
                []


getRows : Grid obj -> List (List ( Point, obj ))
getRows grid =
    let
        minY =
            grid |> Dict.keys |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0

        maxY =
            grid |> Dict.keys |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0
    in
    List.range minY maxY |> List.map (getRow grid)


getRow : Grid obj -> Int -> List ( Point, obj )
getRow grid y =
    grid |> Dict.filter (\( _, py ) _ -> py == y) |> Dict.toList |> List.sortBy Tuple.first
