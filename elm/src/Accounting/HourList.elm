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
        , textInput
        , timeInput
        )
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Common.DateUtil as DateUtil
import Common.ModalDialog as DLG
import Common.Util as Util
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Json.Encode as JE
import Task
import Time


mainUrl : String
mainUrl =
    "/hourlist"


initUrl : String
initUrl =
    mainUrl ++ "/latestdata"


saveUrl : String
saveUrl =
    mainUrl ++ "/insert"


newInvoiceUrl : String
newInvoiceUrl =
    mainUrl ++ "/newinvoice"


saveNewInvoiceUrl : String
saveNewInvoiceUrl =
    mainUrl ++ "/insertinvoice"


saveFakturaposterUrl : String
saveFakturaposterUrl =
    mainUrl ++ "/savefakturaposter"


saveNewGroupUrl : String
saveNewGroupUrl =
    mainUrl ++ "/newgroup"


fetchHourListItemsUrl : String
fetchHourListItemsUrl =
    mainUrl ++ "/hourlistitems"


type alias FakturaposterModel =
    { fromDate : Maybe String
    , toDate : Maybe String
    , hours : Float
    , hourRate : Int
    , desc : String
    }


type alias InvoiceModel =
    { fnr : Maybe Int
    , invoiceDate : Maybe String
    , dueDate : Maybe String
    , desc : String
    , companyIds : List SelectItem
    , companyId : Maybe Int
    }



{-
    oid | fnr |    dato    | timer | gruppe |      gruppe_spes       | spesifikasjon | fra_klokken | til_klokken
   -----+-----+------------+-------+--------+------------------------+---------------+-------------+-------------
      1 | 211 | 2009-03-17 |  7.50 |      1 | Shift Manager, support |               | 00:00       | 00:00
      2 | 211 | 2009-03-18 |  8.00 |      1 | Shift Manager, support |               | 00:00       | 00:00
      3 | 211 | 2009-03-19 |  9.00 |      1 | Shift Manager, support |               | 00:00       | 00:00
-}


type alias HourListItem =
    { oid : Int
    , fnr : Int
    , hdate : String
    , hours : Float
    , group : String
    , desc : Maybe String
    , fromTime : String
    , toTime : String
    }


type alias Model =
    { invoices : List SelectItem
    , invoice : Maybe Int
    , hourlistGroups : List SelectItem
    , hourlistGroup : Maybe Int
    , newHourlistGroup : Maybe String
    , dato : Maybe String
    , fromHour : String
    , toHour : String
    , hours : Maybe Float
    , negativeHours : Maybe Float
    , hdesc : Maybe String
    , resultHours : Maybe Float
    , myStatus : MyStatus
    , dlgNewGroup : DLG.DialogState
    , dlgNewInvoice : DLG.DialogState
    , dlgFakturaposter : DLG.DialogState
    , newInvoice : InvoiceModel
    , fakturaposterModel : FakturaposterModel
    , items : List HourListItem
    }


type MyStatus
    = None
    | MyError String
    | MySuccess String


type NewGroupMsg
    = NewGroupChanged String
    | NewGroupDialog
    | NewGroupOk
    | NewGroupCancel
    | NewGroupSaved (Result Http.Error JsonStatus)


type NewInvoiceMsg
    = FnrChanged String
    | InvDateChanged String
    | DueDateChanged String
    | DescChanged String
    | CompanyIdChanged String
    | NewInvoiceDataFetched (Result Http.Error InvoiceModel)
    | NewInvoiceDialog
    | NewInvoiceOk
    | NewInvoiceCancel
    | NewInvoiceSaved (Result Http.Error JsonStatus)


type FakturaposterMsg
    = FakturaposterDialog
    | FakturaposterOk
    | FakturaposterCancel
    | FromDateChanged String
    | ToDateChanged String
    | InvoiceHoursChanged String
    | HourRateChanged String
    | InvoiceDescChanged String
    | FakturaposterSaved (Result Http.Error JsonStatus)


type Msg
    = InvoiceChanged String
    | InvoiceChangedParamsInvalid ()
    | HourListGroupChanged String
    | HourListItemsFetched (Result Http.Error (List HourListItem))
    | DateChanged String
    | FromHourChanged String
    | ToHourChanged String
    | HoursChanged String
    | NegHoursChanged String
    | HourListDescChanged String
    | InitDataFetched (Result Http.Error InitData)
    | SetTodayDate Time.Posix
    | Save
    | DataSaved (Result Http.Error JsonStatus)
    | SaveToDbParamsInvalid ()
    | NewGroupMsgFor NewGroupMsg
    | NewInvoiceMsgFor NewInvoiceMsg
    | FakturaposterMsgFor FakturaposterMsg



{-
   | NewInvoiceDialog
   | NewInvoiceOk
   | NewInvoiceCancel
   | NewInvoiceSaved (Result Http.Error JsonStatus)
-}


initInvoiceModel : InvoiceModel
initInvoiceModel =
    { fnr = Nothing
    , invoiceDate = Nothing
    , dueDate = Nothing
    , desc = ""
    , companyIds = []
    , companyId = Nothing
    }


initFakturaposterModel : FakturaposterModel
initFakturaposterModel =
    { fromDate = Nothing
    , toDate = Nothing
    , hours = 0.0
    , hourRate = 810
    , desc = "-"
    }


setFnr : Maybe Int -> InvoiceModel -> InvoiceModel
setFnr fnr model =
    { model | fnr = fnr }


setDesc : String -> InvoiceModel -> InvoiceModel
setDesc s model =
    { model | desc = s }


setInvoiceDate : String -> InvoiceModel -> InvoiceModel
setInvoiceDate s model =
    { model | invoiceDate = Just s }


setDueDate : String -> InvoiceModel -> InvoiceModel
setDueDate s model =
    { model | dueDate = Just s }


setCompanyId : String -> InvoiceModel -> InvoiceModel
setCompanyId s model =
    { model | companyId = String.toInt s }


setFromDate : String -> FakturaposterModel -> FakturaposterModel
setFromDate s model =
    { model | fromDate = Just s }


setToDate : String -> FakturaposterModel -> FakturaposterModel
setToDate s model =
    { model | toDate = Just s }


setInvoiceHours : String -> FakturaposterModel -> FakturaposterModel
setInvoiceHours s model =
    { model | hours = Maybe.withDefault 0.0 (String.toFloat s) }


setHourRate : String -> FakturaposterModel -> FakturaposterModel
setHourRate s model =
    { model | hourRate = Maybe.withDefault 0 (String.toInt s) }


setInvoiceDesc : String -> FakturaposterModel -> FakturaposterModel
setInvoiceDesc s model =
    { model | desc = s }



{-
   asDescIn : InvoiceModel -> String -> InvoiceModel
   asDescIn =
       Util.flip setDesc
-}


asInvoiceModelIn : Model -> InvoiceModel -> Model
asInvoiceModelIn model invoice =
    { model | newInvoice = invoice }


asFakturaposterModelIn : Model -> FakturaposterModel -> Model
asFakturaposterModelIn model fm =
    { model | fakturaposterModel = fm }


init : ( Model, Cmd Msg )
init =
    ( { invoices = []
      , invoice = Nothing
      , hourlistGroups = []
      , hourlistGroup = Nothing
      , newHourlistGroup = Nothing
      , dato = Nothing
      , fromHour = "08:00"
      , toHour = "16:00"
      , hours = Just 8
      , negativeHours = Just 0.0
      , hdesc = Nothing
      , resultHours = Just 8
      , myStatus = None
      , dlgNewGroup = DLG.DialogHidden
      , dlgNewInvoice = DLG.DialogHidden
      , dlgFakturaposter = DLG.DialogHidden
      , newInvoice = initInvoiceModel
      , fakturaposterModel = initFakturaposterModel
      , items = []
      }
    , Cmd.batch [ setTodayDate, fetchInitData ]
    )


type alias JsonStatus =
    { ok : Bool
    , msg : String
    , oid : Int

    --, statuscode : Int
    --, items : List Receipt
    }


statusDecoder : JD.Decoder JsonStatus
statusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "oid" JD.int


hourListRow : HourListItem -> Table.Row Msg
hourListRow r =
    let
        hdesc =
            Maybe.withDefault "-" r.desc
    in
    Table.tr []
        [ Table.td [] [ H.text (String.fromInt r.fnr) ]
        , Table.td [] [ H.text r.hdate ]
        , Table.td [] [ H.text (String.fromFloat r.hours) ]
        , Table.td [] [ H.text r.group ]
        , Table.td [] [ H.text hdesc ]
        , Table.td [] [ H.text r.fromTime ]
        , Table.td [] [ H.text r.toTime ]
        ]


curHourListItems : { r | items : List HourListItem } -> H.Html Msg
curHourListItems model =
    let
        rows =
            List.map hourListRow model.items
    in
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ H.text "Fnr" ]
                , Table.th [] [ H.text "Dato" ]
                , Table.th [] [ H.text "Timer" ]
                , Table.th [] [ H.text "Gruppe" ]
                , Table.th [] [ H.text "Spesifikasjon" ]
                , Table.th [] [ H.text "Fra" ]
                , Table.th [] [ H.text "Til" ]
                ]
        , tbody =
            Table.tbody [] rows
        }


view : Model -> H.Html Msg
view model =
    let
        selectedInvoice =
            case model.invoice of
                Nothing ->
                    Nothing

                Just i ->
                    Just <| String.fromInt i

        inv =
            -- bootstrapSelect InvoiceChanged "Fakturanr" model.invoices
            makeSelect InvoiceChanged "Faktura" model.invoices selectedInvoice

        {-
           Select.select
               [ Select.id "invoice"
               , Select.onChange InvoiceChanged
               ]
               [ Select.item [ A.value "-" ] [ H.text "-" ]
               , Select.item [ A.value "393" ] [ H.text "KDO" ]
               , Select.item [ A.value "397" ] [ H.text "Edux" ]
               ]
        -}
        hlg =
            --bootstrapSelect HourlistGroupChanged "Gruppe" model.hourlistGroups
            makeSelect HourListGroupChanged "Gruppe" model.hourlistGroups Nothing

        dx =
            -- bootstrapDate "Dato"
            dateInput DateChanged (LabelText "Dato") model.dato

        fromHour =
            timeInput FromHourChanged (LabelText "Fra") (Just model.fromHour)

        toHour =
            timeInput ToHourChanged (LabelText "To") (Just model.toHour)

        hours =
            numberInput HoursChanged (LabelText "Timer") (Maybe.map String.fromFloat model.resultHours)

        neghours =
            numberInput NegHoursChanged (LabelText "Pause") (Maybe.map String.fromFloat model.negativeHours)

        hourListDesc =
            textInput HourListDescChanged (LabelText "Spesifikasjon") model.hdesc

        btnOk =
            button Save Success "Lagre" True

        btnNewGroup =
            button (NewGroupMsgFor NewGroupDialog) Success "Ny gruppe" True

        btnNewInvoice =
            button (NewInvoiceMsgFor NewInvoiceDialog) Success "Ny faktura" True

        btnFakturaposter =
            button (FakturaposterMsgFor FakturaposterDialog) Success "Fakturaposter" True

        statusHtml : String -> String -> H.Html Msg
        statusHtml elmClass text =
            gridItem (GridPosition "e2")
                (H.div []
                    [ H.p [ A.class elmClass ] [ H.text text ]
                    ]
                )

        items =
            let
                itemsx =
                    [ gridItem (GridPosition "a1") inv
                    , gridItem (GridPosition "b1") hlg
                    , gridItem (GridPosition "c1") dx
                    , gridItem (GridPosition "d1") fromHour
                    , gridItem (GridPosition "e1") toHour
                    , gridItem (GridPosition "a2") hours
                    , gridItem (GridPosition "b2") neghours
                    , gridItem (GridPosition "c2") hourListDesc
                    , gridItem (GridPosition "d2") (H.div [ A.class "flex" ] [ btnOk, btnNewGroup, btnNewInvoice, btnFakturaposter ])
                    , gridItem (GridPosition "f12") (curHourListItems model)
                    ]
            in
            case model.myStatus of
                None ->
                    itemsx

                MyError err ->
                    statusHtml "elm-error" err :: itemsx

                MySuccess s ->
                    statusHtml "elm-success" s :: itemsx
    in
    H.div []
        [ H.div
            [ A.class "accounting-grid" ]
            items
        , newGroupDialog model
        , newInvoiceDialog model
        , fakturaposterDialog model
        ]


newInvoiceDialog : Model -> H.Html Msg
newInvoiceDialog model =
    let
        invModel =
            model.newInvoice

        fnr =
            numberInput (NewInvoiceMsgFor << FnrChanged) (LabelText "Fakturanr") (Maybe.map String.fromInt invModel.fnr)

        invDate =
            dateInput (NewInvoiceMsgFor << InvDateChanged) (LabelText "Fakturadato") invModel.invoiceDate

        dueDate =
            dateInput (NewInvoiceMsgFor << DueDateChanged) (LabelText "Forfall") invModel.dueDate

        desc =
            textInput (NewInvoiceMsgFor << DescChanged) (LabelText "Spesifikasjon") (Just invModel.desc)

        companyId =
            makeSelect (NewInvoiceMsgFor << CompanyIdChanged) "Firma id" invModel.companyIds Nothing
    in
    DLG.modalDialog "Ny Faktura"
        model.dlgNewInvoice
        (NewInvoiceMsgFor NewInvoiceOk)
        (NewInvoiceMsgFor NewInvoiceCancel)
        [ fnr, invDate, dueDate, desc, companyId ]


newGroupDialog : Model -> H.Html Msg
newGroupDialog model =
    let
        newGroupInput =
            textInput (NewGroupMsgFor << NewGroupChanged) (LabelText "Timelistegruppe") model.newHourlistGroup
    in
    DLG.modalDialog "Ny Timelistegruppe"
        model.dlgNewGroup
        (NewGroupMsgFor NewGroupOk)
        (NewGroupMsgFor NewGroupCancel)
        [ newGroupInput ]



{-
    oid |    dato    | sluttdato  | fakturanr | antall | enh_sats |  enh  |     spesifikasjon
   -----+------------+------------+-----------+--------+----------+-------+-----------------------
      1 | 2006-09-25 | 2006-10-01 |       146 |  40.00 |   500.00 | timer | Konsulenttimer uke 39
      5 | 2006-07-15 | 2006-09-30 |       147 |  60.00 |   420.00 | timer | HBMN Fase 2
      2 | 2006-10-02 | 2006-10-08 |       146 |  38.50 |   500.00 | timer | Konsulenttimer uke 40
-}


fakturaposterDialog : Model -> H.Html Msg
fakturaposterDialog model =
    let
        myModel =
            model.fakturaposterModel

        title =
            case model.invoice of
                Nothing ->
                    "Fakturaposter"

                Just t ->
                    "Fakturaposter: " ++ String.fromInt t

        fromDate =
            dateInput (FakturaposterMsgFor << FromDateChanged) (LabelText "Fra dato") myModel.fromDate

        toDate =
            dateInput (FakturaposterMsgFor << ToDateChanged) (LabelText "Til dato") myModel.toDate

        hours =
            numberInput (FakturaposterMsgFor << InvoiceHoursChanged) (LabelText "Timer") (Just <| String.fromFloat myModel.hours)

        hourRate =
            numberInput (FakturaposterMsgFor << HourRateChanged) (LabelText "Timesats") (Just <| String.fromInt myModel.hourRate)

        desc =
            textInput (FakturaposterMsgFor << InvoiceDescChanged) (LabelText "Spesifikasjon") (Just myModel.desc)
    in
    DLG.modalDialog title
        model.dlgFakturaposter
        (FakturaposterMsgFor FakturaposterOk)
        (FakturaposterMsgFor FakturaposterCancel)
        [ fromDate, toDate, hours, hourRate, desc ]


calcHours : String -> String -> Float
calcHours fromH toH =
    Util.hourStrDiff fromH toH


updateNewGroup : NewGroupMsg -> Model -> ( Model, Cmd Msg )
updateNewGroup msg model =
    case msg of
        NewGroupChanged s ->
            let
                curGroup =
                    if String.length s == 0 then
                        Nothing

                    else
                        Just s
            in
            ( { model | newHourlistGroup = curGroup }, Cmd.none )

        NewGroupDialog ->
            ( { model | dlgNewGroup = DLG.DialogVisible }, Cmd.none )

        NewGroupOk ->
            case model.newHourlistGroup of
                Nothing ->
                    ( { model | dlgNewGroup = DLG.DialogHidden, myStatus = None }, Cmd.none )

                Just s ->
                    ( model, saveNewGroup s )

        NewGroupCancel ->
            ( { model | dlgNewGroup = DLG.DialogHidden, myStatus = None }, Cmd.none )

        NewGroupSaved (Ok status) ->
            let
                txt =
                    Maybe.withDefault "N/A" model.newHourlistGroup

                oids =
                    String.fromInt status.oid

                newGroup =
                    SelectItem oids (oids ++ " - " ++ txt)

                newHourlistGroups =
                    newGroup :: model.hourlistGroups
            in
            ( { model | dlgNewGroup = DLG.DialogHidden, hourlistGroups = newHourlistGroups }, Cmd.none )

        NewGroupSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )


updateNewInvoice : NewInvoiceMsg -> Model -> ( Model, Cmd Msg )
updateNewInvoice msg model =
    case msg of
        FnrChanged s ->
            ( model.newInvoice |> setFnr (String.toInt s) |> asInvoiceModelIn model, Cmd.none )

        InvDateChanged s ->
            ( model.newInvoice |> setInvoiceDate s |> asInvoiceModelIn model, Cmd.none )

        DueDateChanged s ->
            ( model.newInvoice |> setDueDate s |> asInvoiceModelIn model, Cmd.none )

        DescChanged s ->
            ( model.newInvoice |> setDesc s |> asInvoiceModelIn model, Cmd.none )

        CompanyIdChanged s ->
            --( { model | hourlistGroup = String.toInt s }, Cmd.none )
            ( model.newInvoice |> setCompanyId s |> asInvoiceModelIn model, Cmd.none )

        NewInvoiceDialog ->
            --( { model | dlgNewInvoice = DLG.DialogVisible }, Cmd.none )
            ( model, fetchNewInvoiceData )

        NewInvoiceDataFetched (Ok data) ->
            ( { model | dlgNewInvoice = DLG.DialogVisible, newInvoice = data }, Cmd.none )

        NewInvoiceDataFetched (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        NewInvoiceOk ->
            ( { model | dlgNewInvoice = DLG.DialogHidden, myStatus = None }, saveNewInvoice model.newInvoice )

        NewInvoiceCancel ->
            ( { model | dlgNewInvoice = DLG.DialogHidden, myStatus = None }, Cmd.none )

        NewInvoiceSaved (Ok _) ->
            let
                oids =
                    String.fromInt <| Maybe.withDefault 0 model.newInvoice.fnr

                newInvoice =
                    SelectItem oids (oids ++ " - " ++ model.newInvoice.desc)

                newInvoices =
                    newInvoice :: model.invoices
            in
            ( { model | invoices = newInvoices }, Cmd.none )

        NewInvoiceSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )


updateFakturaposter : FakturaposterMsg -> Model -> ( Model, Cmd Msg )
updateFakturaposter msg model =
    case msg of
        FakturaposterDialog ->
            ( { model | dlgFakturaposter = DLG.DialogVisible }, Cmd.none )

        FakturaposterOk ->
            ( { model | dlgFakturaposter = DLG.DialogHidden }, saveFakturaposter model.invoice model.fakturaposterModel )

        FakturaposterCancel ->
            ( { model | dlgFakturaposter = DLG.DialogHidden }, Cmd.none )

        FromDateChanged s ->
            ( model.fakturaposterModel |> setFromDate s |> asFakturaposterModelIn model, Cmd.none )

        ToDateChanged s ->
            ( model.fakturaposterModel |> setToDate s |> asFakturaposterModelIn model, Cmd.none )

        InvoiceHoursChanged s ->
            ( model.fakturaposterModel |> setInvoiceHours s |> asFakturaposterModelIn model, Cmd.none )

        HourRateChanged s ->
            ( model.fakturaposterModel |> setHourRate s |> asFakturaposterModelIn model, Cmd.none )

        InvoiceDescChanged s ->
            ( model.fakturaposterModel |> setInvoiceDesc s |> asFakturaposterModelIn model, Cmd.none )

        FakturaposterSaved (Ok _) ->
            ( model, Cmd.none )

        FakturaposterSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )


sumHourListItems : List HourListItem -> Float
sumHourListItems items =
    List.sum <| List.map .hours items


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InvoiceChanged s ->
            let
                invoiceId =
                    String.toInt s

                newModel =
                    { model | invoice = invoiceId }
            in
            ( newModel, fetchHourListItems invoiceId )

        InvoiceChangedParamsInvalid _ ->
            ( { model | items = [] }, Cmd.none )

        HourListGroupChanged s ->
            ( { model | hourlistGroup = String.toInt s }, Cmd.none )

        HourListItemsFetched (Ok data) ->
            let
                sumMsg =
                    "Sum timer: " ++ String.fromFloat (sumHourListItems data)
            in
            ( { model | items = data, myStatus = MySuccess sumMsg }, Cmd.none )

        HourListItemsFetched (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        DateChanged s ->
            ( { model | dato = Just s }, Cmd.none )

        FromHourChanged s ->
            let
                calculated =
                    Just <| calcHours s model.toHour

                resH =
                    Maybe.map2 (\v1 v2 -> v1 - v2) calculated model.negativeHours
            in
            ( { model | fromHour = s, hours = calculated, resultHours = resH }, Cmd.none )

        ToHourChanged s ->
            let
                calculated =
                    Just <| calcHours model.fromHour s

                resH =
                    Maybe.map2 (\v1 v2 -> v1 - v2) calculated model.negativeHours
            in
            ( { model | toHour = s, hours = calculated, resultHours = resH }, Cmd.none )

        HoursChanged s ->
            let
                h =
                    String.toFloat s
            in
            ( { model | hours = h, resultHours = h, negativeHours = Just 0.0 }, Cmd.none )

        NegHoursChanged s ->
            let
                newNegH =
                    String.toFloat s
            in
            ( { model | negativeHours = newNegH, resultHours = Maybe.map2 (\v1 v2 -> v1 - v2) model.hours newNegH }, Cmd.none )

        HourListDescChanged s ->
            ( { model | hdesc = Just s }, Cmd.none )

        InitDataFetched (Ok data) ->
            ( { model | invoices = data.invoices, hourlistGroups = data.hourlistGroups }, Cmd.none )

        InitDataFetched (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        SetTodayDate tm ->
            ( { model | dato = Just <| DateUtil.todayISO8601 tm }, Cmd.none )

        Save ->
            ( model, saveToDb model )

        DataSaved (Ok status) ->
            --( { model | myStatus = MySuccess (String.fromInt status.oid) }, fetchHourListItems model )
            ( model, fetchHourListItems model.invoice )

        DataSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        SaveToDbParamsInvalid _ ->
            ( { model | myStatus = MyError "SaveToDb params invalid" }, Cmd.none )

        NewGroupMsgFor groupMsg ->
            updateNewGroup groupMsg model

        NewInvoiceMsgFor invoiceMsg ->
            updateNewInvoice invoiceMsg model

        FakturaposterMsgFor fakturaposterMsg ->
            updateFakturaposter fakturaposterMsg model


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


newInvoiceDecoder : JD.Decoder InvoiceModel
newInvoiceDecoder =
    JD.succeed InvoiceModel
        |> JP.required "fnr" (JD.nullable JD.int)
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded ""
        |> JP.required "companyid" (JD.list selectItemDecoder)
        |> JP.hardcoded Nothing


fetchNewInvoiceData : Cmd Msg
fetchNewInvoiceData =
    Http.send (NewInvoiceMsgFor << NewInvoiceDataFetched) <| Http.get newInvoiceUrl newInvoiceDecoder


setTodayDate : Cmd Msg
setTodayDate =
    Task.perform SetTodayDate Time.now



{-
      type alias HourListItemsJson =
          { items : List HourListItem
          }
   hourListItemsJsonDecoder : JD.Decoder HourListItemsJson
   hourListItemsJsonDecoder =
       JD.succeed HourListItemsJson
           |> JP.required "items" (JD.list hourListItemDecoder)
-}


hourListItemDecoder : JD.Decoder HourListItem
hourListItemDecoder =
    JD.succeed HourListItem
        |> JP.required "oid" JD.int
        |> JP.required "fnr" JD.int
        |> JP.required "hdate" JD.string
        |> JP.required "hours" JD.float
        |> JP.required "group" JD.string
        |> JP.required "desc" (JD.nullable JD.string)
        |> JP.required "fromtime" JD.string
        |> JP.required "totime" JD.string


fetchHourListItems : Maybe Int -> Cmd Msg
fetchHourListItems invoice =
    case invoice of
        Nothing ->
            Task.perform InvoiceChangedParamsInvalid (Task.succeed ())

        Just invoice1 ->
            let
                myUrl =
                    fetchHourListItemsUrl ++ "/" ++ String.fromInt invoice1
            in
            Http.send HourListItemsFetched <| Http.get myUrl (JD.list hourListItemDecoder)


saveNewGroup : String -> Cmd Msg
saveNewGroup newGroup =
    let
        params =
            [ ( "name", JE.string newGroup )
            ]

        jbody =
            Util.asHttpBody params
    in
    Http.send (NewGroupMsgFor << NewGroupSaved) <|
        Http.post saveNewGroupUrl jbody statusDecoder


saveFakturaposter : Maybe Int -> FakturaposterModel -> Cmd Msg
saveFakturaposter fnr model =
    let
        params =
            fnr
                |> Maybe.andThen
                    (\fnr1 ->
                        model.fromDate
                            |> Maybe.andThen
                                (\fromDate1 ->
                                    model.toDate
                                        |> Maybe.andThen
                                            (\toDate1 ->
                                                Just
                                                    [ ( "fnr", JE.int fnr1 )
                                                    , ( "fromdate", JE.string fromDate1 )
                                                    , ( "todate", JE.string toDate1 )
                                                    , ( "hours", JE.float model.hours )
                                                    , ( "hourrate", JE.int model.hourRate )
                                                    , ( "desc", JE.string model.desc )
                                                    ]
                                            )
                                )
                    )
    in
    case params of
        Nothing ->
            Task.perform SaveToDbParamsInvalid (Task.succeed ())

        Just params1 ->
            let
                jbody =
                    Util.asHttpBody params1
            in
            Http.send (FakturaposterMsgFor << FakturaposterSaved) <| Http.post saveFakturaposterUrl jbody statusDecoder


saveNewInvoice : InvoiceModel -> Cmd Msg
saveNewInvoice model =
    let
        params =
            model.fnr
                |> Maybe.andThen
                    (\fnr1 ->
                        model.invoiceDate
                            |> Maybe.andThen
                                (\invoiceDate1 ->
                                    model.dueDate
                                        |> Maybe.andThen
                                            (\dueDate1 ->
                                                model.companyId
                                                    |> Maybe.andThen
                                                        (\companyId1 ->
                                                            Just
                                                                [ ( "fnr", JE.int fnr1 )
                                                                , ( "date", JE.string invoiceDate1 )
                                                                , ( "duedate", JE.string dueDate1 )
                                                                , ( "desc", JE.string model.desc )
                                                                , ( "companyid", JE.int companyId1 )
                                                                ]
                                                        )
                                            )
                                )
                    )
    in
    case params of
        Nothing ->
            Task.perform SaveToDbParamsInvalid (Task.succeed ())

        Just params1 ->
            let
                jbody =
                    Util.asHttpBody params1
            in
            Http.send (NewInvoiceMsgFor << NewInvoiceSaved) <| Http.post saveNewInvoiceUrl jbody statusDecoder


saveToDb :
    { r
        | invoice : Maybe Int
        , hourlistGroup : Maybe Int
        , dato : Maybe String
        , fromHour : String
        , toHour : String
        , hours : Maybe Float
        , hdesc : Maybe String
    }
    -> Cmd Msg
saveToDb model =
    let
        params =
            model.invoice
                |> Maybe.andThen
                    (\invoice1 ->
                        model.hourlistGroup
                            |> Maybe.andThen
                                (\hourlistGroup1 ->
                                    model.dato
                                        |> Maybe.andThen
                                            (\dato1 ->
                                                model.hours
                                                    |> Maybe.andThen
                                                        (\hours1 ->
                                                            let
                                                                stem =
                                                                    [ ( "fnr", JE.int invoice1 )
                                                                    , ( "group", JE.int hourlistGroup1 )
                                                                    , ( "curdate", JE.string dato1 )
                                                                    , ( "fromtime", JE.string model.fromHour )
                                                                    , ( "totime", JE.string model.toHour )
                                                                    , ( "hours", JE.float hours1 )
                                                                    ]
                                                            in
                                                            case model.hdesc of
                                                                Nothing ->
                                                                    Just stem

                                                                Just desc1 ->
                                                                    Just (( "desc", JE.string desc1 ) :: stem)
                                                        )
                                            )
                                )
                    )
    in
    case params of
        Nothing ->
            Task.perform SaveToDbParamsInvalid (Task.succeed ())

        Just params1 ->
            let
                jbody =
                    Util.asHttpBody params1
            in
            Http.send DataSaved <|
                Http.post saveUrl jbody statusDecoder



{-
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
-}
