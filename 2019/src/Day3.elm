module Day3 exposing (Model, Msg, init, initAction, name, update, view)

import Advent
import Components as C
import Dict exposing (Dict)
import Exec
import Grid
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


name =
    "Day 3: Crossed Wires"


exampleInputs : List Advent.ExampleInput
exampleInputs =
    [ { name = "Example 1", input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" }
    , { name = "Example 2", input = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" }
    ]



-- Common


parseInput : String -> ( Wire, Wire )
parseInput s =
    case String.lines s of
        l1 :: l2 :: ls ->
            ( parseWire l1, parseWire l2 )

        _ ->
            ( [], [] )


parseWire : String -> Wire
parseWire =
    String.split "," >> List.map parseMove


parseMove : String -> Move
parseMove s =
    let
        chr =
            String.left 1 s

        len =
            String.dropLeft 1 s |> String.toInt |> Maybe.withDefault 0
    in
    case chr of
        "R" ->
            Right len

        "L" ->
            Left len

        "U" ->
            Up len

        "D" ->
            Down len

        _ ->
            -- Whatever.
            Right len



-- Solve Part A


type Move
    = Left Int
    | Right Int
    | Up Int
    | Down Int


type alias Wire =
    List Move


pointsOnWire : Wire -> Dict Grid.Point Int
pointsOnWire wire =
    pointsOnWireRec wire ( 0, 0 ) 0


pointsOnWireRec : Wire -> Grid.Point -> Int -> Dict Grid.Point Int
pointsOnWireRec wire loc len =
    case wire of
        [] ->
            Dict.empty

        mv :: mvs ->
            let
                mvPts =
                    pointsAlongMove loc mv

                newLoc =
                    List.reverse mvPts |> List.head |> Maybe.withDefault ( 0, 0 )
            in
            mvPts
                |> List.indexedMap (\ix pt -> ( pt, len + ix + 1 ))
                |> Dict.fromList
                |> (\pts -> Dict.union pts (pointsOnWireRec mvs newLoc (len + List.length mvPts)))


pointsAlongMove : Grid.Point -> Move -> List Grid.Point
pointsAlongMove start move =
    let
        v =
            moveVelocity move
    in
    moveDistance move
        |> List.range 1
        |> List.map (\x -> Grid.scalarMul x v |> Grid.pointAdd start)


moveVelocity : Move -> Grid.Point
moveVelocity move =
    case move of
        Left _ ->
            ( -1, 0 )

        Right _ ->
            ( 1, 0 )

        Up _ ->
            ( 0, 1 )

        Down _ ->
            ( 0, -1 )


moveDistance : Move -> Int
moveDistance move =
    case move of
        Left d ->
            d

        Right d ->
            d

        Up d ->
            d

        Down d ->
            d


intersections : Wire -> Wire -> List ( Grid.Point, Int )
intersections w1 w2 =
    let
        pts1 =
            pointsOnWire w1

        pts2 =
            pointsOnWire w2
    in
    Dict.toList <|
        Dict.merge
            (\p d -> identity)
            (\p d1 d2 -> Dict.insert p (d1 + d2))
            (\p d -> identity)
            pts1
            pts2
            Dict.empty


type StateA
    = InProgressA Wire Wire
    | AnswerA Int


initA : String -> StateA
initA =
    parseInput >> (\( w1, w2 ) -> InProgressA w1 w2)


stepA : Exec.StepFunction StateA
stepA state =
    case state of
        InProgressA w1 w2 ->
            let
                ans =
                    intersections w1 w2
                        |> List.map Tuple.first
                        |> List.map (Grid.manhattan ( 0, 0 ))
                        |> List.minimum
                        |> Maybe.withDefault 0
            in
            ( AnswerA ans, True )

        _ ->
            ( state, True )



-- Solve Part B


type StateB
    = InProgressB Wire Wire
    | AnswerB Int


initB : String -> StateB
initB =
    parseInput >> (\( w1, w2 ) -> InProgressB w1 w2)


stepB : Exec.StepFunction StateB
stepB state =
    case state of
        InProgressB w1 w2 ->
            let
                ans =
                    intersections w1 w2
                        |> List.map Tuple.second
                        |> List.minimum
                        |> Maybe.withDefault 0
            in
            ( AnswerB ans, True )

        _ ->
            ( state, True )



-- Model


type alias Model =
    Advent.Model StateA StateB


init =
    Advent.init


initAction =
    Advent.loadSourceFile "Day3.elm"


type alias Msg =
    Advent.Msg StateA StateB


update =
    Advent.update



-- View


view : Model -> Html Msg
view model =
    div []
        [ C.title name
        , C.adventOfCodeProblemLink 2019 3
        , C.largeProblemInput "Enter input here" model.input Advent.SetInput
        , C.loadExampleButtons exampleInputs
        , C.section "Part A"
            [ text "I first calculate all the coordinates on each wire, along with the corresponding distance along each wire.\nThen intersect the two sets and find the closest to the origin using manhattan distance."
            , C.partASource model
            , div [ class "flex flex-column justify-center items-center" ]
                [ C.controlProcessButton model.processA Advent.ControlA (initA model.input) stepA
                , viewProgressA model
                ]
            ]
        , C.section "Part B"
            [ text "Reusing most of the code from part A, I simply take the minimum distance along the wires, instead of the closest point to the origin."
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
