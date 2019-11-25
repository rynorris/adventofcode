module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)

import Day1
import Day2


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type ChallengeId
    = Day1Id
    | Day2Id


type alias Model =
    { selectedChallengeId : ChallengeId
    , day1 : Day1.Model
    , day2 : Day2.Model
    }


init : Model
init =
    { selectedChallengeId = Day1Id
    , day1 = Day1.init
    , day2 = Day2.init
    }


-- UPDATE


type Msg
  = SelectChallengeId ChallengeId
  | Day1 Day1.Msg
  | Day2 Day2.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    SelectChallengeId challenge ->
        { model | selectedChallengeId = challenge }
    Day1 subMsg ->
        { model | day1 = (Day1.update subMsg model.day1) }
    Day2 subMsg ->
        { model | day2 = (Day2.update subMsg model.day2) }


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex w-100 vh-100 items-center justify-center pa4 bg-washed-green sans-serif" ]
    [ div [ class "flex w-100 h-100 mw8 shadow-2 br3 bg-white" ]
        [ Html.node "link" [ rel "stylesheet", href "https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css" ] []
        , div [ class "w5 h-100 br bw1 b--black-10" ]
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
menuItemClass challenge selectedChallengeId
    = "w-100 pa2 shadow-hover hover-bg-green bb bw1 b--black-10" ++ (if challenge == selectedChallengeId then " bg-light-green" else "")


renderProblem : Model -> Html Msg
renderProblem model
    = case model.selectedChallengeId of
        Day1Id -> (Html.map Day1) (Day1.view model.day1)
        Day2Id -> (Html.map Day2) (Day2.view model.day2)


challengeName : ChallengeId -> String
challengeName id
    = case id of
        Day1Id -> "1. " ++ Day1.name
        Day2Id -> "2. " ++ "Day 2 with a really super duper long name"
