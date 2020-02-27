module Accounting.Update exposing (update, updateUrl)

import Accounting.GeneralJournal as GeneralJournal
import Accounting.HourList as HourList
import Accounting.Types
    exposing
        ( Model
        , Msg(..)
        , Page(..)
        , Route(..)
        )
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


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



{-
   ( { model | page = GalleryPage gallery }
    , Cmd.map GotGalleryMsg cmd
    )
-}


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map GeneralJournalRoute (s "generaljournal") -- Parser.top
        , Parser.map HourListRoute (s "hourlist")

        --, Parser.map Gallery (s "gallery")
        ]



{-
   case model.page of
       GalleryPage gallery ->
           toGallery model (Gallery.update galleryMsg gallery)

       _ ->
           ( model, Cmd.none )

-}
