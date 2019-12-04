module Main exposing (..)

import Advent
import Browser
import Browser.Navigation as Nav
import Day1
import Day2
import Day3
import Day4
import Home
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Practice
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
    = Home
    | PagePractice
    | PageDay1
    | PageDay2
    | PageDay3
    | PageDay4


type alias Model =
    { key : Nav.Key
    , selectedPageId : PageId
    , practice : Practice.Model
    , day1 : Day1.Model
    , day2 : Day2.Model
    , day3 : Day3.Model
    , day4 : Day4.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        pageId =
            Maybe.withDefault Home (parseRoute url)
    in
    ( { key = key
      , selectedPageId = pageId
      , practice = Practice.init
      , day1 = Day1.init
      , day2 = Day2.init
      , day3 = Day3.init
      , day4 = Day4.init
      }
    , initAction pageId
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectPage PageId
    | Practice Practice.Msg
    | Day1 Day1.Msg
    | Day2 Day2.Msg
    | Day3 Day3.Msg
    | Day4 Day4.Msg


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

        Practice subMsg ->
            let
                ( new, subCmd ) =
                    Practice.update subMsg model.practice
            in
            ( { model | practice = new }, Cmd.map Practice subCmd )

        Day1 subMsg ->
            let
                ( new, subCmd ) =
                    Day1.update subMsg model.day1
            in
            ( { model | day1 = new }, Cmd.map Day1 subCmd )

        Day2 subMsg ->
            let
                ( new, subCmd ) =
                    Day2.update subMsg model.day2
            in
            ( { model | day2 = new }, Cmd.map Day2 subCmd )

        Day3 subMsg ->
            let
                ( new, subCmd ) =
                    Day3.update subMsg model.day3
            in
            ( { model | day3 = new }, Cmd.map Day3 subCmd )

        Day4 subMsg ->
            let
                ( new, subCmd ) =
                    Day4.update subMsg model.day4
            in
            ( { model | day4 = new }, Cmd.map Day4 subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2019"
    , body =
        [ div [ class "flex w-100 vh-100 items-center justify-center pa4 bg-near-black moon-gray sans-serif" ]
            [ div [ class "flex w-100 h-100 mw8 br3 ba bw1 b--dark-green collapse overflow-auto" ]
                [ div [ class "w5 h-100 br bw1 b--dark-green collapse" ]
                    [ menuItem Home model
                    , menuItem PagePractice model
                    , menuItem PageDay1 model
                    , menuItem PageDay2 model
                    , menuItem PageDay3 model
                    , menuItem PageDay4 model
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
        Home ->
            Home.view

        PagePractice ->
            Html.map Practice (Practice.view model.practice)

        PageDay1 ->
            Html.map Day1 (Day1.view model.day1)

        PageDay2 ->
            Html.map Day2 (Day2.view model.day2)

        PageDay3 ->
            Html.map Day3 (Day3.view model.day3)

        PageDay4 ->
            Html.map Day4 (Day4.view model.day4)


pageName : PageId -> String
pageName id =
    case id of
        Home ->
            "Home"

        PagePractice ->
            Practice.name

        PageDay1 ->
            Day1.name

        PageDay2 ->
            Day2.name

        PageDay3 ->
            Day3.name

        PageDay4 ->
            Day4.name


pageUrl : PageId -> String
pageUrl id =
    case id of
        Home ->
            "/"

        PagePractice ->
            "/practice"

        PageDay1 ->
            "/day1"

        PageDay2 ->
            "/day2"

        PageDay3 ->
            "/day3"

        PageDay4 ->
            "/day4"


initAction : PageId -> Cmd Msg
initAction id =
    case id of
        Home ->
            Cmd.none

        PagePractice ->
            Practice.initAction |> Cmd.map Practice

        PageDay1 ->
            Day1.initAction |> Cmd.map Day1

        PageDay2 ->
            Day2.initAction |> Cmd.map Day2

        PageDay3 ->
            Day3.initAction |> Cmd.map Day3

        PageDay4 ->
            Day4.initAction |> Cmd.map Day4


navigateTo : PageId -> Model -> Cmd Msg
navigateTo id model =
    Cmd.batch
        [ Nav.pushUrl model.key (pageUrl id)
        , initAction id
        ]


parser : Parser (PageId -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map PagePractice (Parser.s "practice")
        , Parser.map PageDay1 (Parser.s "day1")
        , Parser.map PageDay2 (Parser.s "day2")
        , Parser.map PageDay3 (Parser.s "day3")
        , Parser.map PageDay4 (Parser.s "day4")
        ]


parseRoute : Url.Url -> Maybe PageId
parseRoute =
    Parser.parse parser
