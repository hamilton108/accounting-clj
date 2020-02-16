module GeneralJournal.View exposing (view)

import Browser
import GeneralJournal.Types
    exposing
        ( Model
        , Msg(..)
        )
import Html as H
import Html.Attributes as A
import Html.Events as E


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


view :
    Model
    -> Browser.Document Msg -- H.Html Msg
view model =
    { title = "General Journal SPA"
    , body =
        []
    }
