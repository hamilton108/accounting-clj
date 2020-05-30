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


saveNewGroupUrl : String
saveNewGroupUrl =
    mainUrl ++ "/newgroup"


fetchHourListItemsUrl : String
fetchHourListItemsUrl =
    mainUrl ++ "/hourlistitems"


fakturaposterUrl : String
fakturaposterUrl =
    mainUrl ++ "/fakturaposter"


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
    , newInvoice : InvoiceModel
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


type Msg
    = InvoiceChanged String
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
    | Fakturaposter
    | FakturaposterSaved (Result Http.Error JsonStatus)



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



{-
   asDescIn : InvoiceModel -> String -> InvoiceModel
   asDescIn =
       Util.flip setDesc
-}


asInvoiceModelIn : Model -> InvoiceModel -> Model
asInvoiceModelIn model invoice =
    { model | newInvoice = invoice }


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
      , newInvoice = initInvoiceModel
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
        inv =
            -- bootstrapSelect InvoiceChanged "Fakturanr" model.invoices
            makeSelect InvoiceChanged "Faktura" model.invoices Nothing

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
            button Fakturaposter Success "Fakturaposter" True

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
            ( newModel, fetchHourListItems newModel )

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

        DataSaved (Ok _) ->
            --( { model | myStatus = MySuccess (String.fromInt status.oid) }, fetchHourListItems model )
            ( model, fetchHourListItems model )

        DataSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        SaveToDbParamsInvalid _ ->
            ( { model | myStatus = MyError "SaveToDb params invalid" }, Cmd.none )

        NewGroupMsgFor groupMsg ->
            updateNewGroup groupMsg model

        NewInvoiceMsgFor invoiceMsg ->
            updateNewInvoice invoiceMsg model

        Fakturaposter ->
            ( model, saveToFakturaposter model )

        FakturaposterSaved (Ok status) ->
            ( model, Cmd.none )

        FakturaposterSaved (Err err) ->
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


fetchHourListItems : { r | invoice : Maybe Int } -> Cmd Msg
fetchHourListItems model =
    case model.invoice of
        Nothing ->
            Task.perform SaveToDbParamsInvalid (Task.succeed ())

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


saveToFakturaposter :
    { r
        | invoice : Maybe Int
        , items : List HourListItem
    }
    -> Cmd Msg
saveToFakturaposter model =
    let
        params =
            model.invoice
                |> Maybe.andThen
                    (\invoice1 ->
                        Just
                            [ ( "fnr", JE.int invoice1 )
                            , ( "sum", JE.float (sumHourListItems model.items) )
                            ]
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
            Http.send FakturaposterSaved <| Http.post fakturaposterUrl jbody statusDecoder


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
