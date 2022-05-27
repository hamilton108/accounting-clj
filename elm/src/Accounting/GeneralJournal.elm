module Accounting.GeneralJournal exposing (Model, Msg(..), init, receiptRow, update, view)

import Accounting.Ui
    exposing
        ( BootstrapButton(..)
        , GridPosition(..)
        , LabelText(..)
        , SelectItem
        , SelectItems
        , button
        , dateInput
        , gridItem
        , makeSelect
        , numberInput
        , textInput
        )
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Table as Table
import Common.Util as Util
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Json.Encode as JE
import Task


mainUrl : String
mainUrl =
    "/generaljournal"


initUrl : String
initUrl =
    mainUrl ++ "/latestdata"


saveUrl : String
saveUrl =
    mainUrl ++ "/insert"


type alias Field =
    Maybe String


type alias FloatField =
    Maybe Float


type alias IntField =
    Maybe Int


type MyStatus
    = None
    | MyError String
    | Oid Int


type alias Receipt =
    { bilag : Int
    , date : String
    , debit : Int
    , text : String
    , amount : Float
    }


type alias Model =
    { ns4102 : SelectItems
    , lastBilagDate : String
    , bilag : Int
    , date : Field
    , desc : Field
    , belop : Field 
    , mvaAmount : FloatField
    , mva : Bool
    , selectedNs4102 : IntField
    , myStatus : MyStatus
    , incBilag : Bool
    , lastReceipts : List Receipt
    }


myPresets : SelectItems
myPresets =
    [ { val = "1", txt = "Taxi" }
    , { val = "2", txt = "Datautstyr" }
    , { val = "3", txt = "NextGenTel" }
    , { val = "4", txt = "NetCom" }
    , { val = "5", txt = "Telenor" }
    , { val = "6", txt = "Rudshøgda boligsameie" }
    , { val = "7", txt = "Hafslund" }
    , { val = "8", txt = "Lunsj" }
    , { val = "9", txt = "Overtidsmat" }
    , { val = "10", txt = "Ruter mnd kort" }
    , { val = "11", txt = "Ruter" }
    , { val = "12", txt = "OSL" }
    ]


type Msg
    = Save
    | DateChanged String
    | DebitChanged String
    | PresetChanged String
    | DescChanged String
    | BilagChanged String
    | BelopChanged String
    | MvaChanged String
    | InitDataFetched (Result Http.Error Model)
    | DataSaved (Result Http.Error JsonStatus)
    | IsMva25Changed Bool
    | IncBilagChanged Bool
    | SaveToDbParamsInvalid () --Time.Posix


init : ( Model, Cmd Msg )
init =
    ( { ns4102 = []
      , lastBilagDate = ""
      , bilag = 0
      , date = Nothing
      , desc = Nothing
      , belop = Nothing
      , mvaAmount = Nothing
      , mva = True
      , selectedNs4102 = Nothing
      , myStatus = None
      , incBilag = True
      , lastReceipts = []
      }
    , fetchInitData
    )



--receiptRow : Receipt ->


receiptRow : Receipt -> Table.Row Msg
receiptRow r =
    Table.tr []
        [ Table.td [] [ H.text (String.fromInt r.bilag) ]
        , Table.td [] [ H.text r.date ]
        , Table.td [] [ H.text (String.fromInt r.debit) ]
        , Table.td [] [ H.text r.text ]
        , Table.td [] [ H.text (String.fromFloat r.amount) ]
        ]


curLastReceipts : { r | lastReceipts : List Receipt } -> H.Html Msg
curLastReceipts model =
    let
        rows =
            List.map receiptRow model.lastReceipts
    in
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ H.text "Bilag" ]
                , Table.th [] [ H.text "Dato" ]
                , Table.th [] [ H.text "Debet" ]
                , Table.th [] [ H.text "Tekst" ]
                , Table.th [] [ H.text "Beløp" ]
                ]
        , tbody =
            Table.tbody [] rows
        }


view : Model -> H.Html Msg
view model =
    let
        curdate =
            dateInput DateChanged (LabelText "Dato") model.date

        debet =
            makeSelect DebitChanged "Debet" model.ns4102 (Maybe.map String.fromInt model.selectedNs4102)

        presets =
            makeSelect PresetChanged "Preset" myPresets Nothing

        desc =
            textInput DescChanged (LabelText "Tekst") model.desc

        bilag =
            numberInput BilagChanged (LabelText "Bilag") (Just (String.fromInt model.bilag))

        belop =
            textInput BelopChanged (LabelText "Beløp") model.belop
            -- numberInput BelopChanged (LabelText "Beløp") (Maybe.map String.fromFloat model.belop)

        mvaAmount =
            numberInput MvaChanged (LabelText "Mva beløp") (Maybe.map String.fromFloat model.mvaAmount)

        isMva25 =
            Checkbox.checkbox
                [ Checkbox.id "cb-mva"
                , Checkbox.checked model.mva
                , Checkbox.onCheck IsMva25Changed
                ]
                "Mva 25 %"

        incBilag =
            Checkbox.checkbox
                [ Checkbox.id "cb-inc"
                , Checkbox.checked model.incBilag
                , Checkbox.onCheck IncBilagChanged
                ]
                "Do increment bilag"

        btnOk =
            button Save Success "Lagre" True

        lastItems =
            curLastReceipts model

        btnGridItem : String -> String -> H.Html Msg
        btnGridItem elmClass text =
            gridItem (GridPosition "d2")
                (H.div [ A.class "flex" ]
                    [ btnOk
                    , H.p [ A.class elmClass ] [ H.text text ]
                    ]
                )

        items =
            let
                itemsx =
                    [ gridItem (GridPosition "a1") curdate
                    , gridItem (GridPosition "b1") debet
                    , gridItem (GridPosition "c1") desc
                    , gridItem (GridPosition "d1") bilag
                    , gridItem (GridPosition "e1") belop
                    , gridItem (GridPosition "a2") presets
                    , gridItem (GridPosition "b2") mvaAmount
                    , gridItem (GridPosition "c2") (H.div [] [ isMva25, incBilag ])
                    , gridItem (GridPosition "e2") lastItems
                    ]
            in
            case model.myStatus of
                None ->
                    gridItem (GridPosition "d2") btnOk
                        :: itemsx

                MyError err ->
                    btnGridItem "elm-error" err
                        :: itemsx

                Oid oid ->
                    btnGridItem "elm-ok" ("Saved with oid: " ++ String.fromInt oid)
                        :: itemsx
    in
    H.div [ A.class "accounting-grid" ] items


str2float : String -> FloatField
str2float s = 
    let 
        sx = String.replace "," "." s
    in
    String.toFloat sx

calcMvaAmount : Bool -> Field -> FloatField
calcMvaAmount cb belop =
    if cb == False then
        Nothing

    else
        case belop of 
            Nothing -> Nothing

            Just belop2 -> 
                let 
                    belopx = str2float belop2
                in
                Maybe.map ((*) 0.2) belopx


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Save ->
            ( model, saveToDb model )

        DebitChanged s ->
            ( { model | selectedNs4102 = String.toInt s }, Cmd.none )

        PresetChanged s ->
            let
                newModel =
                    updatePreset model s

                curMvaAmount =
                    calcMvaAmount newModel.mva newModel.belop
            in
            ( { newModel | mvaAmount = curMvaAmount }, Cmd.none )

        DateChanged s ->
            ( { model | date = Just s }, Cmd.none )

        DescChanged s ->
            ( { model | desc = Just s }, Cmd.none )

        BilagChanged s ->
            let
                curBilag =
                    Maybe.withDefault 0 (String.toInt s)
            in
            ( { model | bilag = curBilag }, Cmd.none )

        BelopChanged s ->
            let
                newBelop =
                    Just s
                    -- String.toFloat s

                curMvaAmount =
                    calcMvaAmount model.mva newBelop
            in
            ( { model | belop = newBelop, mvaAmount = curMvaAmount }, Cmd.none )

        MvaChanged s ->
            ( { model | mvaAmount = String.toFloat s }, Cmd.none )

        InitDataFetched (Ok initData) ->
            ( initData, Cmd.none )

        InitDataFetched (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        DataSaved (Ok json) ->
            let
                curBilag =
                    if model.incBilag == True then
                        model.bilag + 1

                    else
                        model.bilag
            in
            ( { model | bilag = curBilag, myStatus = Oid json.statuscode, lastReceipts = json.items }, Cmd.none )

        DataSaved (Err err) ->
            ( { model | myStatus = MyError (Util.httpErr2str err) }, Cmd.none )

        IsMva25Changed cb ->
            let
                curMvaAmount =
                    calcMvaAmount cb model.belop
            in
            ( { model | mva = cb, mvaAmount = curMvaAmount }, Cmd.none )

        IncBilagChanged cb ->
            ( { model | incBilag = cb }, Cmd.none )

        SaveToDbParamsInvalid _ ->
            ( { model | myStatus = MyError "SaveToDb params invalid" }, Cmd.none )


updatePreset : Model -> String -> Model
updatePreset model preset =
    let
        ( curDebit, curDesc, curMva ) =
            case preset of
                "1" ->
                    ( Just 7140, "Taxi", False )

                "2" ->
                    ( Just 6581, "Datautstyr", True )

                "3" ->
                    ( Just 6910, "NextGenTel", True )

                "4" ->
                    ( Just 6900, "NetCom", True )

                "5" ->
                    ( Just 6900, "Telenor", True )

                "6" ->
                    ( Just 6300, "Rudshøgda boligsameie", False )

                "7" ->
                    ( Just 6340, "Hafslund", False )

                "8" ->
                    ( Just 7160, "Lunsj", False )

                "9" ->
                    ( Just 7160, "Overtidsmat", False )

                "10" ->
                    ( Just 7140, "Ruter mnd kort", False )

                "11" ->
                    ( Just 7140, "Ruter", False )

                "12" ->
                    ( Just 7140, "OSL", False )

                _ ->
                    ( Nothing, "", False )
    in
    { model
        | selectedNs4102 = curDebit
        , desc = Just curDesc
        , mva = curMva
    }


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2 SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)



{-
   21 (defn last-receipts []
   22   (map (fn [^GeneralJournalBean x]
   23         {:bilag (str (.getBilag x))
   24          :date (.getTransactionDateStr x)
   25          :debit (str (.getDebit x))
   26          :credit (str (.getCredit x))
   27          :text (.getText x)
   28          :amount (str (.getAmount x))})
   29      (DBX/fetch-by-bilag)))
-}


receiptDecoder : JD.Decoder Receipt
receiptDecoder =
    JD.succeed Receipt
        |> JP.required "bilag" JD.int
        |> JP.required "date" JD.string
        |> JP.required "debit" JD.int
        |> JP.required "text" JD.string
        |> JP.required "amount" JD.float


initDataDecoder : JD.Decoder Model
initDataDecoder =
    JD.succeed Model
        |> JP.required "ns4102" (JD.list selectItemDecoder)
        |> JP.required "bilag-dx" JD.string
        |> JP.required "bilag" JD.int
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded True
        |> JP.hardcoded Nothing
        |> JP.hardcoded None
        |> JP.hardcoded True
        |> JP.required "items" (JD.list receiptDecoder)


fetchInitData : Cmd Msg
fetchInitData =
    Http.send InitDataFetched <| Http.get initUrl initDataDecoder



--(PUT "/insert" [bilag curdate debit desc amount mva]


type alias JsonStatus =
    { ok : Bool
    , msg : String
    , statuscode : Int
    , items : List Receipt
    }


statusDecoder : JD.Decoder JsonStatus
statusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "statuscode" JD.int
        |> JP.required "items" (JD.list receiptDecoder)


saveToDb :
    { r
        | bilag : Int
        , date : Field
        , selectedNs4102 : IntField
        , desc : Field
        , belop : Field
        , mvaAmount : FloatField
    }
    -> Cmd Msg
saveToDb model =
    let
        mva =
            Maybe.withDefault 0.0 model.mvaAmount

        params =
            model.date
                |> Maybe.andThen
                    (\dx ->
                        model.selectedNs4102
                            |> Maybe.andThen
                                (\ns4102 ->
                                    model.desc
                                        |> Maybe.andThen
                                            (\dsc ->
                                                model.belop
                                                    |> Maybe.andThen
                                                        (\blp ->
                                                            Just
                                                                [ ( "bilag", JE.int model.bilag )
                                                                , ( "curdate", JE.string dx )
                                                                , ( "debit", JE.int ns4102 )
                                                                , ( "desc", JE.string dsc )
                                                                , ( "amount", JE.string blp )
                                                                , ( "mva", JE.float mva )
                                                                ]
                                                        )
                                            )
                                )
                    )
    in
    case params of
        Nothing ->
            Task.perform SaveToDbParamsInvalid (Task.succeed ())

        -- emptyTask
        Just params1 ->
            let
                jbody =
                    Util.asHttpBody params1
            in
            Http.send DataSaved <|
                Http.post saveUrl jbody statusDecoder
