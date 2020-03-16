module Accounting.GeneralJournal exposing (Model, Msg(..), init, update, view)

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
import Common.Util as Util
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Json.Encode as JE


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
    | HttpError String
    | Oid Int


type alias Model =
    { ns4102 : SelectItems
    , lastBilagDate : String
    , bilag : Int
    , date : Field
    , desc : Field
    , belop : FloatField
    , mvaAmount : FloatField
    , mva : Bool
    , selectedNs4102 : IntField
    , myStatus : MyStatus
    }


myPresets : SelectItems
myPresets =
    [ { val = "1", txt = "Taxi" }
    , { val = "2", txt = "Datautstyr" }
    , { val = "3", txt = "NextGenTel" }
    , { val = "4", txt = "NetCom" }
    , { val = "5", txt = "Telenor" }
    , { val = "6", txt = "Obos" }
    , { val = "7", txt = "Hafslund" }
    , { val = "8", txt = "Lunsj" }
    , { val = "9", txt = "Overtidsmat" }
    , { val = "10", txt = "Ruter mnd kort" }
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
      }
    , fetchInitData
    )


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
            numberInput BelopChanged (LabelText "Beløp") (Maybe.map String.fromFloat model.belop)

        mvaAmount =
            numberInput MvaChanged (LabelText "Mva beløp") (Maybe.map String.fromFloat model.mvaAmount)

        isMva25 =
            Checkbox.checkbox
                [ Checkbox.id "cb-mva"
                , Checkbox.checked model.mva
                , Checkbox.onCheck IsMva25Changed
                ]
                "Mva 25 %"

        btnOk =
            button Save Success "Lagre" True

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
                    , gridItem (GridPosition "c2") isMva25
                    , gridItem (GridPosition "d2") btnOk
                    ]
            in
            case model.myStatus of
                None ->
                    itemsx

                HttpError err ->
                    gridItem (GridPosition "e2") (H.p [ A.class "elm-error" ] [ H.text err ]) :: itemsx

                Oid oid ->
                    gridItem (GridPosition "e2") (H.p [ A.class "elm-ok" ] [ H.text ("Saved with oid: " ++ String.fromInt oid) ]) :: itemsx
    in
    H.div [ A.class "accounting-grid" ] items


calcMvaAmount : Bool -> FloatField -> FloatField
calcMvaAmount cb belop =
    if cb == False then
        Nothing

    else
        Maybe.map ((*) 0.25) belop


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
                curMvaAmount =
                    calcMvaAmount model.mva model.belop
            in
            ( { model | belop = String.toFloat s, mvaAmount = curMvaAmount }, Cmd.none )

        MvaChanged s ->
            ( { model | mvaAmount = String.toFloat s }, Cmd.none )

        InitDataFetched (Ok initData) ->
            ( initData, Cmd.none )

        InitDataFetched (Err err) ->
            ( { model | myStatus = HttpError (Util.httpErr2str err) }, Cmd.none )

        DataSaved (Ok jsonStatus) ->
            ( { model | bilag = model.bilag + 1, myStatus = Oid jsonStatus.statuscode }, Cmd.none )

        DataSaved (Err err) ->
            ( { model | myStatus = HttpError (Util.httpErr2str err) }, Cmd.none )

        IsMva25Changed cb ->
            let
                curMvaAmount =
                    calcMvaAmount cb model.belop
            in
            ( { model | mva = cb, mvaAmount = curMvaAmount }, Cmd.none )


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
                    ( Just 6300, "OBOS", False )

                "7" ->
                    ( Just 6340, "Hafslund", False )

                "8" ->
                    ( Just 7160, "Lunsj", False )

                "9" ->
                    ( Just 7160, "Overtidsmat", False )

                "10" ->
                    ( Just 7140, "Ruter mnd kort", False )

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


fetchInitData : Cmd Msg
fetchInitData =
    Http.send InitDataFetched <| Http.get initUrl initDataDecoder



--(PUT "/insert" [bilag curdate debit desc amount mva]


type alias JsonStatus =
    { ok : Bool
    , msg : String
    , statuscode : Int
    }


statusDecoder : JD.Decoder JsonStatus
statusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "statuscode" JD.int


saveToDb :
    { r
        | bilag : Int
        , date : Field
        , selectedNs4102 : IntField
        , desc : Field
        , belop : FloatField
        , mvaAmount : FloatField
    }
    -> Cmd Msg
saveToDb model =
    let
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
                                                            model.mvaAmount
                                                                |> Maybe.andThen
                                                                    (\mva ->
                                                                        Just
                                                                            [ ( "bilag", JE.int model.bilag )
                                                                            , ( "curdate", JE.string dx )
                                                                            , ( "debit", JE.int ns4102 )
                                                                            , ( "desc", JE.string dsc )
                                                                            , ( "amount", JE.float blp )
                                                                            , ( "mva", JE.float mva )
                                                                            ]
                                                                    )
                                                        )
                                            )
                                )
                    )
    in
    case params of
        Nothing ->
            Cmd.none

        Just params1 ->
            let
                jbody =
                    Util.asHttpBody params1
            in
            Http.send DataSaved <|
                Http.post saveUrl jbody statusDecoder



{-
   let
       (Bilag b1) =
           model.bilag

       params =
           model.date
               |> Maybe.andThen
                   (\dx ->
                       model.selectedNs4102
                           |> Maybe.andThen
                               (\ns4102 ->
                                   model.desc
                                       |> Maybe.andThen
                                           (\de ->
                                               model.belop
                                                   |> Maybe.andThen
                                                       (\be ->
                                                           model.mvaAmount
                                                               |> Maybe.andThen
                                                                   (\mva ->
                                                                       Just
                                                                           [ ( "bilag", JE.string b1 )
                                                                           , ( "curdate", JE.string dx )
                                                                           , ( "debit", JE.int ns )
                                                                           , ( "desc", JE.string "dsfsdfsf" )
                                                                           , ( "amount", JE.float 12.2 )
                                                                           , ( "mva", JE.float 3.34 )
                                                                           ]
                                                                   )
                                                       )
                                           )
                               )
                   )
   in
   case
       params
   of
       Just params1 ->
           let
               jbody =
                   Util.asHttpBody params1
           in
           Http.send DataSaved <|
               Http.post saveUrl jbody statusDecoder

       Nothing ->
           Cmd.none

-}
