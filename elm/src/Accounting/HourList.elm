module Accounting.HourList exposing (Model, Msg(..), init, update, view)

import Accounting.Ui
    exposing
        ( GridPosition(..)
        , SelectItem
        , bootstrapEmptySelect
        , bootstrapSelect
        , gridItem
        )
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select exposing (Item)
import Common.Util as Util
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Json.Encode as JE


mainUrl : String
mainUrl =
    "/hourlist"


initUrl : String
initUrl =
    mainUrl ++ "/latestdata"


type alias Model =
    { invoices : List (Item Msg)
    }


type Msg
    = Noop
    | InvoiceChanged String
    | InitDataFetched (Result Http.Error InitData)


init : ( Model, Cmd Msg )
init =
    ( { invoices = [] }, fetchInitData )


view : Model -> H.Html Msg
view model =
    let
        invoice =
            bootstrapSelect InvoiceChanged "Fakturanr" model.invoices

        items =
            [ gridItem (GridPosition "a1") invoice ]
    in
    H.div [ A.class "accounting-grid" ] items


toBootstrapSelect : SelectItem -> Item Msg
toBootstrapSelect s =
    Select.item
        [ A.value s.val ]
        [ H.text s.txt ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InvoiceChanged s ->
            ( model, Cmd.none )

        InitDataFetched (Ok data) ->
            let
                (InitData initData) =
                    data

                curInvoices =
                    bootstrapEmptySelect :: List.map toBootstrapSelect initData
            in
            ( { model | invoices = curInvoices }, Cmd.none )

        InitDataFetched (Err err) ->
            Debug.log (Util.httpErr2str err)
                ( model, Cmd.none )


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2 SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)


type InitData
    = InitData (List SelectItem)


initDataDecoder : JD.Decoder InitData
initDataDecoder =
    JD.succeed InitData
        |> JP.required "invoices" (JD.list selectItemDecoder)


fetchInitData : Cmd Msg
fetchInitData =
    Http.send InitDataFetched <| Http.get initUrl initDataDecoder
