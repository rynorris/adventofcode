module Main exposing (..)

import Browser
import Day1
import Day2
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type ChallengeId
    = Day1Id
    | Day2Id


type alias Model =
    { selectedChallengeId : ChallengeId
    , day1 : Day1.Model
    , day2 : Day2.Model
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { selectedChallengeId = Day1Id
      , day1 = Day1.init
      , day2 = Day2.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectChallengeId ChallengeId
    | Day1 Day1.Msg
    | Day2 Day2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectChallengeId challenge ->
            ( { model | selectedChallengeId = challenge }, Cmd.none )

        Day1 subMsg ->
            let
                ( newDay1, subCmd ) =
                    Day1.update subMsg model.day1
            in
            ( { model | day1 = newDay1 }, Cmd.map Day1 subCmd )

        Day2 subMsg ->
            ( { model | day2 = Day2.update subMsg model.day2 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex w-100 vh-100 items-center justify-center pa4 bg-near-black moon-gray sans-serif" ]
        [ div [ class "flex w-100 h-100 mw8 br3 ba b--dark-green overflow-auto" ]
            [ Html.node "link" [ rel "stylesheet", href "https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css" ] []
            , div [ class "w5 h-100 br bw1 b--dark-green" ]
                [ menuItem Day1Id model.selectedChallengeId
                , menuItem Day2Id model.selectedChallengeId
                ]
            , div [ class "w-100 h-100 pa2 overflow-auto" ] [ renderProblem model ]
            ]
        ]


menuItem : ChallengeId -> ChallengeId -> Html Msg
menuItem challenge selectedChallengeId =
    div [ class (menuItemClass challenge selectedChallengeId), onClick (SelectChallengeId challenge) ] [ text (challengeName challenge) ]


menuItemClass : ChallengeId -> ChallengeId -> String
menuItemClass challenge selectedChallengeId =
    "w-100 pa2 shadow-hover bb bw1 b--dark-green"
        ++ (if challenge == selectedChallengeId then
                " bg-dark-green"

            else
                " hover-bg-dark-green"
           )


renderProblem : Model -> Html Msg
renderProblem model =
    case model.selectedChallengeId of
        Day1Id ->
            Html.map Day1 (Day1.view model.day1)

        Day2Id ->
            Html.map Day2 (Day2.view model.day2)


challengeName : ChallengeId -> String
challengeName id =
    case id of
        Day1Id ->
            "1. " ++ Day1.name

        Day2Id ->
            "2. " ++ "Day 2 with a really super duper long name"
