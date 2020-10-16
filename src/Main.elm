module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Url
import Url.Parser as Parser exposing ((</>), Parser)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


init flags url key =
    let
        initalModel =
            { key = key, pageState = Home, tally = initalTally }
    in
    processUrl url initalModel


type alias Model =
    { key : Nav.Key, pageState : PageState, tally : Tally }


type PageState
    = Home
    | Link1
    | Link2


type alias Tally =
    { home : Int
    , link1 : Int
    , link2 : Int
    }


initalTally : Tally
initalTally =
    { home = 0, link1 = 0, link2 = 0 }


type Msg
    = UrlChange Url.Url
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            processUrl url model

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


updateForPageHit : PageState -> Model -> Model
updateForPageHit newPageState model =
    let
        oldTally =
            model.tally

        newTally =
            case newPageState of
                Home ->
                    { oldTally | home = oldTally.home + 1 }

                Link1 ->
                    { oldTally | link1 = oldTally.link1 + 1 }

                Link2 ->
                    { oldTally | link2 = oldTally.link2 + 1 }
    in
    { model | pageState = newPageState, tally = newTally }


routeParser : Parser (PageState -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Link1 (Parser.s "link1")
        , Parser.map Link2 (Parser.s "link2")
        ]


processUrl : Url.Url -> Model -> ( Model, Cmd Msg )
processUrl url model =
    case Parser.parse routeParser url of
        Just pageSate ->
            ( updateForPageHit pageSate model, Cmd.none )

        Nothing ->
            ( model, Nav.pushUrl model.key "/" )


view : Model -> Browser.Document Msg
view model =
    { title = "nav test", body = [ viewHeader model, viewbody model ] }


viewHeader : Model -> Html Msg
viewHeader model =
    div []
        [ showGreeting model.pageState
        , text "Here's some links:"
        , Html.ul []
            [ Html.li [] [ a [ Attributes.href "/" ] [ text "home" ] ]
            , Html.li [] [ a [ Attributes.href "/link1" ] [ text "link 1" ] ]
            , Html.li [] [ a [ Attributes.href "link2" ] [ text "link 2" ] ]
            ]
        ]


showGreeting : PageState -> Html Msg
showGreeting pageState =
    Html.h1 [] [ text <| "Welcome to " ++ pageStateString pageState ]


pageStateString : PageState -> String
pageStateString pageState =
    case pageState of
        Home ->
            "Home"

        Link1 ->
            "Link 1"

        Link2 ->
            "Link 2"


viewbody : Model -> Html Msg
viewbody model =
    div []
        [ text "Here's how many times you've visited them"
        , Html.ul []
            [ Html.li [] [ text <| "home: " ++ String.fromInt model.tally.home ]
            , Html.li [] [ text <| "link1: " ++ String.fromInt model.tally.link1 ]
            , Html.li [] [ text <| "link2: " ++ String.fromInt model.tally.link2 ]
            ]
        ]


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    Sub.none
