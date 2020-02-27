module Accounting.HourList exposing (Model, Msg(..), init, update, view)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Url exposing (Url)


type alias Model =
    {}


type Msg
    = Noop


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


view : Model -> H.Html Msg
view model =
    H.div [ A.class "content" ]
        [ H.div [ A.class "folders" ]
            [ H.text "HourList" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )
