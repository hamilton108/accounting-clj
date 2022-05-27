module Accounting.Main exposing (main)

import Accounting.GeneralJournal as GeneralJournal
import Accounting.HourList as HourList
import Accounting.Types
    exposing
        ( Flags
        , Model
        , Msg(..)
        , Page(..)
        , Route(..)
        )
import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s)



--import Url.Parser as Parser exposing ((</>), Parser, s, string)


title : String
title =
    "Accounting SPA - 1.9.2"


type alias Flags =
    Int


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url { page = NotFound, key = key }


view : Model -> Browser.Document Msg
view model =
    let
        content : H.Html Msg
        content =
            case model.page of
                GeneralJournalPage m ->
                    GeneralJournal.view m
                        |> H.map GeneralJournalMsg

                HourListPage m ->
                    HourList.view m
                        |> H.map HourListMsg

                NotFound ->
                    H.text "No page selected"
    in
    { title = title
    , body =
        [ viewHeader model.page, content ]
    }


viewHeader : Page -> H.Html Msg
viewHeader page =
    let
        logo =
            H.h1 [] [ H.text title ]

        links =
            H.ul []
                [ navLink GeneralJournalRoute { url = "/generaljournal", caption = "General Journal" }
                , navLink HourListRoute { url = "/hourlist", caption = "Hourlist" }
                ]

        navLink : Route -> { url : String, caption : String } -> H.Html msg
        navLink route { url, caption } =
            H.li [ A.classList [ ( "active", isActive { link = route, page = page } ) ] ]
                [ H.a [ A.href url ] [ H.text caption ] ]
    in
    H.nav [] [ logo, links ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        --------------------------------------------
        ( GeneralJournalRoute, GeneralJournalPage _ ) ->
            True

        ( GeneralJournalRoute, _ ) ->
            False

        ( HourListRoute, HourListPage _ ) ->
            True

        ( HourListRoute, _ ) ->
            False



{-
   viewFooter : H.Html msg
   viewFooter =
       H.footer [] [ H.text "One is never alone with a rubber duck. -Douglas Adams" ]
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GeneralJournalMsg m ->
            case model.page of
                GeneralJournalPage gjModel ->
                    toGeneralJournal model (GeneralJournal.update m gjModel)

                _ ->
                    ( model, Cmd.none )

        HourListMsg m ->
            case model.page of
                HourListPage hlModel ->
                    toHourList model (HourList.update m hlModel)

                _ ->
                    ( model, Cmd.none )


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just GeneralJournalRoute ->
            GeneralJournal.init
                |> toGeneralJournal model

        Just HourListRoute ->
            HourList.init
                |> toHourList model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


toHourList : Model -> ( HourList.Model, Cmd HourList.Msg ) -> ( Model, Cmd Msg )
toHourList model ( gj, cmd ) =
    ( { model | page = HourListPage gj }, Cmd.map HourListMsg cmd )


toGeneralJournal : Model -> ( GeneralJournal.Model, Cmd GeneralJournal.Msg ) -> ( Model, Cmd Msg )
toGeneralJournal model ( gj, cmd ) =
    ( { model | page = GeneralJournalPage gj }, Cmd.map GeneralJournalMsg cmd )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map GeneralJournalRoute (s "generaljournal") -- Parser.top
        , Parser.map HourListRoute (s "hourlist")
        ]
