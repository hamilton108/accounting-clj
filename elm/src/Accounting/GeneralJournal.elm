module Accounting.GeneralJournal exposing (Model, Msg(..), init, update, view)

import Accounting.Ui exposing (GridPosition(..), gridItem, textInput)
import Html as H
import Html.Attributes as A
import Html.Events as E


type alias Model =
    {}


type Msg
    = Noop


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


view : Model -> H.Html Msg
view model =
    let
        tx =
            textInput
    in
    H.div [ A.class "generaljournal-grid" ]
        [ gridItem (GridPosition "a1") tx
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )
