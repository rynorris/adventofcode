module Day6 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Dict exposing (Dict)
import Exec
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Day 6: Universal Orbit Map"


exampleInputs : List Advent.ExampleInput
exampleInputs =
    []



-- Common


parseInput : String -> Graph
parseInput =
    String.lines >> List.map parseEdge >> edgesToGraph


parseEdge : String -> Edge
parseEdge =
    String.split ")" >> (\ps -> Edge (unsafeGet 0 "" ps) (unsafeGet 1 "" ps))


unsafeGet : Int -> a -> List a -> a
unsafeGet ix def =
    List.drop ix >> List.head >> Maybe.withDefault def



-- Solve Part A


type alias Graph =
    { nodes : Set Node
    , edges : List Edge
    }


type alias Node =
    String


type Edge
    = Edge Node Node


nodeSet : List Edge -> Set Node
nodeSet =
    List.map (\(Edge p c) -> [ p, c ]) >> List.concat >> Set.fromList


edgesToGraph : List Edge -> Graph
edgesToGraph es =
    { nodes = nodeSet es, edges = es }


parent : Edge -> Node
parent (Edge p _) =
    p


child : Edge -> Node
child (Edge _ c) =
    c


getParents : Graph -> Node -> List Node
getParents g n =
    g.edges |> List.filter (\e -> child e == n) |> List.map parent


getChildren : Graph -> Node -> List Node
getChildren g n =
    g.edges |> List.filter (\e -> parent e == n) |> List.map child


orbitCount : Graph -> Int
orbitCount g =
    let
        childMap =
            g.nodes |> Set.toList |> List.map (\n -> ( n, getChildren g n )) |> Dict.fromList

        countRec depth n =
            let
                children =
                    Dict.get n childMap |> Maybe.withDefault []
            in
            depth + (children |> List.map (countRec (depth + 1)) |> List.foldl (+) 0)
    in
    countRec 0 "COM"


type StateA
    = InProgressA Graph
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> InProgressA


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA graph ->
            ( AnswerA (orbitCount graph), True )

        _ ->
            ( state, True )



-- Solve Part B


getNeighbours : Graph -> Node -> List Node
getNeighbours g n =
    List.append (getParents g n) (getChildren g n)


distance : Graph -> Node -> Node -> Int
distance g a b =
    let
        distanceRec seen queue =
            case queue of
                [] ->
                    -1

                ( x, d ) :: xs ->
                    if x == b then
                        d

                    else if Dict.member x seen then
                        distanceRec seen xs

                    else
                        let
                            ns =
                                getNeighbours g x |> List.filter (\n -> Dict.member n seen |> not) |> List.map (\n -> ( n, d + 1 ))
                        in
                        distanceRec (Dict.insert x d seen) (List.append xs ns)
    in
    distanceRec Dict.empty [ ( a, 0 ) ]


type StateB
    = InProgressB Graph
    | AnswerB Int


initB : String -> StateB
initB =
    parseInput >> InProgressB


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB graph ->
            ( AnswerB (distance graph "YOU" "SAN" - 2), True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day6.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update 1000 1000



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 6
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.loadExampleButtons exampleInputs
        , C.section "Part A"
            [ text "For this part I traverse the graph starting from COM, summing the depth of each node."
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "For this part I start from YOU, and traverse throughout the graph naively counting distance until I find SAN.  The graph isn't large enough to necessitate the use of a more complex algorithm."
            , C.partBSource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processB Advent.ControlB (initB model.input) stepB
                , viewProgressB model
                ]
            ]
        , C.sourceCodeLink "Day6.elm"
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
                AnswerB ans ->
                    div [] [ text ("The answer is: " ++ String.fromInt ans) ]

                _ ->
                    div [] []

        _ ->
            div [] []
