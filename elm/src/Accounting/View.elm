module Accounting.View exposing (view)

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
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy)


type GridPosition
    = GridPosition String


gridItem : GridPosition -> H.Html msg -> H.Html msg
gridItem (GridPosition clazz) item =
    H.div [ A.class clazz ] [ item ]


formGroupItem : String -> String -> H.Html msg -> H.Html msg
formGroupItem clazz title myInput =
    H.div [ A.class "form-group" ]
        [ H.label [ A.class clazz ]
            [ H.text title ]
        , myInput
        ]


dateItem : String -> Maybe String -> (String -> msg) -> H.Html msg
dateItem title value event =
    let
        isMissing =
            value == Nothing

        value_ =
            Maybe.withDefault "" value

        dateClazz =
            "form-control date start-date"

        myClazz =
            if isMissing == True then
                "col-form-label missing"

            else
                "col-form-label"

        myInput =
            H.input [ A.value value_, A.type_ "date", A.class dateClazz, E.onInput event ] []
    in
    formGroupItem myClazz title myInput


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
    { title = "Accounting SPA"
    , body =
        [ viewHeader model.page, content, viewFooter ]
    }


viewHeader : Page -> H.Html Msg
viewHeader page =
    let
        logo =
            H.h1 [] [ H.text "Accounting" ]

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


viewFooter : H.Html msg
viewFooter =
    H.footer [] [ H.text "One is never alone with a rubber duck. -Douglas Adams" ]
