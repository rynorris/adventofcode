module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Day1
import Day2
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Url
import Url.Parser as Parser exposing (Parser)



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type PageId
    = PageDay1
    | PageDay2


type alias Model =
    { key : Nav.Key
    , selectedPageId : PageId
    , day1 : Day1.Model
    , day2 : Day2.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , selectedPageId = Maybe.withDefault PageDay1 (parseRoute url)
      , day1 = Day1.init
      , day2 = Day2.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectPage PageId
    | Day1 Day1.Msg
    | Day2 Day2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | selectedPageId = Maybe.withDefault PageDay1 (parseRoute url) }, Cmd.none )

        SelectPage id ->
            ( { model | selectedPageId = id }, navigateTo id model )

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


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2019"
    , body =
        [ div [ class "flex w-100 vh-100 items-center justify-center pa4 bg-near-black moon-gray sans-serif" ]
            [ div [ class "flex w-100 h-100 mw8 br3 ba b--dark-green overflow-auto" ]
                [ div [ class "w5 h-100 br bw1 b--dark-green" ]
                    [ menuItem PageDay1 model
                    , menuItem PageDay2 model
                    ]
                , div [ class "w-100 h-100 pa2 overflow-auto" ] [ renderPage model ]
                ]
            ]
        ]
    }


menuItem : PageId -> Model -> Html Msg
menuItem id model =
    div [ class (menuItemClass id model.selectedPageId), onClick (SelectPage id) ] [ text (pageName id) ]


menuItemClass : PageId -> PageId -> String
menuItemClass id selectedPageId =
    "w-100 pa2 shadow-hover bb bw1 b--dark-green"
        ++ (if id == selectedPageId then
                " bg-dark-green"

            else
                " hover-bg-dark-green"
           )


renderPage : Model -> Html Msg
renderPage model =
    case model.selectedPageId of
        PageDay1 ->
            Html.map Day1 (Day1.view model.day1)

        PageDay2 ->
            Html.map Day2 (Day2.view model.day2)


pageName : PageId -> String
pageName id =
    case id of
        PageDay1 ->
            "1. " ++ Day1.name

        PageDay2 ->
            "2. " ++ "Day 2 with a really super duper long name"


pageUrl : PageId -> String
pageUrl id =
    case id of
        PageDay1 ->
            "/day1"

        PageDay2 ->
            "/day2"


navigateTo : PageId -> Model -> Cmd Msg
navigateTo id model =
    Nav.pushUrl model.key (pageUrl id)


parser : Parser (PageId -> a) a
parser =
    Parser.oneOf
        [ Parser.map PageDay1 (Parser.s "day1")
        , Parser.map PageDay2 (Parser.s "day2")
        ]


parseRoute : Url.Url -> Maybe PageId
parseRoute =
    Parser.parse parser
