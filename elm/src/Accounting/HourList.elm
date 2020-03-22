module Accounting.HourList exposing (Model, Msg(..), init, update, view)

-- import Bootstrap.Form.Checkbox as Checkbox

import Accounting.Ui
    exposing
        ( BootstrapButton(..)
        , GridPosition(..)
        , LabelText(..)
        , SelectItem
        , button
        , dateInput
        , gridItem
        , makeSelect
        , numberInput
        , timeInput
        )
import Common.Util as Util
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP


mainUrl : String
mainUrl =
    "/hourlist"


initUrl : String
initUrl =
    mainUrl ++ "/latestdata"


type alias Model =
    { invoices : List SelectItem
    , invoice : Maybe Int
    , hourlistGroups : List SelectItem
    , hourlistGroup : Maybe Int
    , dato : Maybe String
    , fromHour : String
    , toHour : String
    , hours : Maybe Float
    , negativeHours : Maybe Float
    }


type Msg
    = InvoiceChanged String
    | HourlistGroupChanged String
    | DateChanged String
    | FromHourChanged String
    | ToHourChanged String
    | HoursChanged String
    | InitDataFetched (Result Http.Error InitData)
    | Save


init : ( Model, Cmd Msg )
init =
    ( { invoices = []
      , invoice = Nothing
      , hourlistGroups = []
      , hourlistGroup = Nothing
      , dato = Nothing
      , fromHour = "08:00"
      , toHour = "16:00"
      , hours = Just 7.5
      , negativeHours = Just 0.5
      }
    , fetchInitData
    )


view : Model -> H.Html Msg
view model =
    let
        inv =
            -- bootstrapSelect InvoiceChanged "Fakturanr" model.invoices
            makeSelect InvoiceChanged "Faktura" model.invoices Nothing

        hlg =
            --bootstrapSelect HourlistGroupChanged "Gruppe" model.hourlistGroups
            makeSelect HourlistGroupChanged "Gruppe" model.hourlistGroups Nothing

        dx =
            -- bootstrapDate "Dato"
            dateInput DateChanged (LabelText "Dato") model.dato

        fromHour =
            timeInput FromHourChanged (LabelText "Fra") (Just model.fromHour)

        toHour =
            timeInput ToHourChanged (LabelText "To") (Just model.toHour)

        hours =
            numberInput HoursChanged (LabelText "Timer") (Maybe.map String.fromFloat model.hours)

        neghours =
            numberInput HoursChanged (LabelText "Pause") (Maybe.map String.fromFloat model.negativeHours)

        btnOk =
            button Save Success "Lagre" True

        items =
            [ gridItem (GridPosition "a1") inv
            , gridItem (GridPosition "b1") hlg
            , gridItem (GridPosition "c1") dx
            , gridItem (GridPosition "d1") fromHour
            , gridItem (GridPosition "e1") toHour
            , gridItem (GridPosition "a2") hours
            , gridItem (GridPosition "b2") neghours
            , gridItem (GridPosition "c2") btnOk
            ]
    in
    H.div [ A.class "accounting-grid" ] items


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InvoiceChanged s ->
            Debug.log s
                ( { model | invoice = String.toInt s }, Cmd.none )

        HourlistGroupChanged s ->
            ( { model | hourlistGroup = String.toInt s }, Cmd.none )

        DateChanged s ->
            ( { model | dato = Just s }, Cmd.none )

        FromHourChanged s ->
            ( { model | fromHour = s }, Cmd.none )

        ToHourChanged s ->
            ( { model | toHour = s }, Cmd.none )

        HoursChanged s ->
            ( { model | hours = String.toFloat s }, Cmd.none )

        InitDataFetched (Ok data) ->
            ( { model | invoices = data.invoices, hourlistGroups = data.hourlistGroups }, Cmd.none )

        InitDataFetched (Err err) ->
            Debug.log (Util.httpErr2str err)
                ( model, Cmd.none )

        Save ->
            ( model, Cmd.none )


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2 SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)


type alias InitData =
    { invoices : List SelectItem
    , hourlistGroups : List SelectItem
    }


initDataDecoder : JD.Decoder InitData
initDataDecoder =
    JD.succeed InitData
        |> JP.required "invoices" (JD.list selectItemDecoder)
        |> JP.required "hourlistgroups" (JD.list selectItemDecoder)


fetchInitData : Cmd Msg
fetchInitData =
    Http.send InitDataFetched <| Http.get initUrl initDataDecoder
