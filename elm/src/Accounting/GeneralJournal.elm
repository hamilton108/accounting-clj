module Accounting.GeneralJournal exposing (Model, Msg(..), Tx(..), init, update, view)

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


type Tx
    = Tx (Maybe String)


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


type alias NumField =
    Maybe Float


type Bilag
    = Bilag String


type alias Model =
    { ns4102 : SelectItems
    , lastBilagDate : String
    , bilag : Bilag
    , date : Field
    , desc : Field
    , belop : Field
    , mvaAmount : Field
    , mva : Bool
    , selectedNs4102 : Maybe String
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
      , bilag = Bilag "0"
      , date = Nothing
      , desc = Nothing
      , belop = Nothing
      , mvaAmount = Nothing
      , mva = True
      , selectedNs4102 = Nothing
      }
    , fetchInitData
    )


view : Model -> H.Html Msg
view model =
    let
        curdate =
            dateInput DateChanged (LabelText "Dato") model.date

        debet =
            makeSelect DebitChanged "Debet" model.ns4102 model.selectedNs4102

        presets =
            makeSelect PresetChanged "Preset" myPresets Nothing

        desc =
            textInput DescChanged (LabelText "Tekst") model.desc

        bilag =
            let
                (Bilag curBilag) =
                    model.bilag
            in
            numberInput BilagChanged (LabelText "Bilag") (Just curBilag)

        belop =
            numberInput BelopChanged (LabelText "Beløp") model.belop

        mvaAmount =
            numberInput MvaChanged (LabelText "Mva beløp") model.mvaAmount

        isMva25 =
            Checkbox.checkbox
                [ Checkbox.id "cb-mva"
                , Checkbox.checked model.mva
                , Checkbox.onCheck IsMva25Changed
                ]
                "Mva 25 %"

        btnOk =
            button Save Success "Lagre" True
    in
    H.div [ A.class "accounting-grid" ]
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


httpErr2str : Http.Error -> String
httpErr2str err =
    case err of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadUrl s ->
            "BadUrl: " ++ s

        Http.BadStatus r ->
            "BadStatus: "

        Http.BadPayload s r ->
            "BadPayload: " ++ s


calcMvaAmount : Bool -> Maybe String -> Maybe String
calcMvaAmount cb belop =
    if cb == False then
        Nothing

    else
        case belop of
            Nothing ->
                Nothing

            Just b ->
                let
                    bx =
                        Maybe.withDefault 0 (String.toFloat b)
                in
                Just (String.fromFloat (bx * 0.25))



{-
   stringToInt : Maybe String -> Int
   stringToInt s =
       case s of
           Nothing ->
               Nothing

           Just x ->
               Maybe.withDefault 0 (String.toInt x)


   stringToFloat : Maybe String -> Int
   stringToFloat s =
       case s of
           Nothing ->
               Nothing

           Just x ->
               Maybe.withDefault 0 (String.toFloat x)
-}


model2save : Model -> SaveToDb
model2save model =
    { bilag = 1
    , curdate = "s"
    , debit = 2
    , desc = "sfs"
    , amount = 120.2
    , mva = 12.3
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Save ->
            ( model, saveToDb )

        DebitChanged s ->
            let
                curNs4102 =
                    if s == "" then
                        Nothing

                    else
                        Just s
            in
            ( { model | selectedNs4102 = curNs4102 }, Cmd.none )

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
            ( { model | bilag = Bilag s }, Cmd.none )

        BelopChanged s ->
            let
                curMvaAmount =
                    calcMvaAmount model.mva model.belop
            in
            ( { model | belop = Just s, mvaAmount = curMvaAmount }, Cmd.none )

        MvaChanged s ->
            Debug.log "update"
                ( { model | mvaAmount = Just s }, Cmd.none )

        InitDataFetched (Ok initData) ->
            ( initData, Cmd.none )

        InitDataFetched (Err err) ->
            Debug.log (httpErr2str err)
                ( model, Cmd.none )

        DataSaved (Ok jsonStatus) ->
            Debug.log jsonStatus.msg
                ( model, Cmd.none )

        DataSaved (Err err) ->
            Debug.log (httpErr2str err)
                ( model, Cmd.none )

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
                    ( "7140", "Taxi", False )

                "2" ->
                    ( "6581", "Datautstyr", True )

                "3" ->
                    ( "6910", "NextGenTel", True )

                "4" ->
                    ( "6900", "NetCom", True )

                "5" ->
                    ( "6900", "Telenor", True )

                "6" ->
                    ( "6300", "OBOS", False )

                "7" ->
                    ( "6340", "Hafslund", False )

                "8" ->
                    ( "7160", "Lunsj", False )

                "9" ->
                    ( "7160", "Overtidsmat", False )

                "10" ->
                    ( "7140", "Ruter mnd kort", False )

                _ ->
                    ( "-", "", False )
    in
    { model
        | selectedNs4102 = Just curDebit
        , desc = Just curDesc
        , mva = curMva
    }


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2 SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)


bilagDecoder : JD.Decoder Bilag
bilagDecoder =
    JD.map Bilag
        JD.string


initDataDecoder : JD.Decoder Model
initDataDecoder =
    JD.succeed Model
        |> JP.required "ns4102" (JD.list selectItemDecoder)
        |> JP.required "bilag-dx" JD.string
        |> JP.required "bilag" bilagDecoder
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded True
        |> JP.hardcoded Nothing


fetchInitData : Cmd Msg
fetchInitData =
    Http.send InitDataFetched <| Http.get initUrl initDataDecoder



--(PUT "/insert" [bilag curdate debit desc amount mva]


type alias SaveToDb =
    { bilag : Int
    , curdate : String
    , debit : Int
    , desc : String
    , amount : Float
    , mva : Float
    }


type alias JsonStatus =
    { ok : Bool, msg : String, statuscode : Int }


statusDecoder : JD.Decoder JsonStatus
statusDecoder =
    JD.succeed JsonStatus
        |> JP.required "ok" JD.bool
        |> JP.required "msg" JD.string
        |> JP.required "statuscode" JD.int



--saveToDb : SaveToDb -> Cmd Msg
--saveToDb { bilag, curdate, debit, desc, amount, mva } =


saveToDb : Cmd Msg
saveToDb =
    let
        d =
            { bilag = 1
            , curdate = "s"
            , debit = 2
            , desc = "sfs"
            , amount = 120.2
            , mva = 12.3
            }

        params =
            [ ( "bilag", JE.int d.bilag )
            , ( "curdate", JE.string d.curdate )
            , ( "debit", JE.int d.debit )
            , ( "desc", JE.string d.desc )
            , ( "amount", JE.float d.amount )
            , ( "mva", JE.float d.mva )
            ]

        jbody =
            Util.asHttpBody params
    in
    Http.send DataSaved <|
        Http.post saveUrl jbody statusDecoder



{-
   {"ns4102":
       [{"name":"1500 - Kundefordringer","value":"1500"},
       {"name":"1900 - Bankinnskudd, kontanter og lignende","value":"1900"},
       {"name":"1901 - Kontanter","value":"1901"},
       {"name":"1902 - Bankkonto 8601.11.16932","value":"1902"},
       {"name":"2070 - Skatter","value":"2070"},
       {"name":"2700 - Utgående mva.","value":"2700"},
       {"name":"2710 - Inngående mva","value":"2710"},
       {"name":"2711 - Inngående mva, høy sats","value":"2711"},
       {"name":"2713 - Inngående mva, middels sats","value":"2713"},
       {"name":"2714 - Inngående mva, lav sats","value":"2714"},
       {"name":"2740 - Oppgjørskonto mva.","value":"2740"},
       {"name":"3700 - Provisjonsinntekt","value":"3700"},
       {"name":"6300 - Leie lokale","value":"6300"},
       {"name":"6340 - Lys, varme","value":"6340"},
       {"name":"6490 - Annen leiekostnad","value":"6490"},
       {"name":"6510 - Håndverktøy","value":"6510"},
       {"name":"6581 - Datautstyr","value":"6581"},
       {"name":"6700 - Revisjons- og regnskapshonorar","value":"6700"},
       {"name":"6790 - Annen fremmed tjeneste","value":"6790"},
       {"name":"6800 - Kontorrekvisita","value":"6800"},
       {"name":"6840 - Aviser, tidsskrifter, bøker o.l.","value":"6840"},
       {"name":"6841 - Amazon.com","value":"6841"},
       {"name":"6842 - Annet","value":"6842"},
       {"name":"6843 - apress.com","value":"6843"},
       {"name":"6844 - manning.com","value":"6844"},
       {"name":"6860 - Møte, kurs, oppdatering o.l.","value":"6860"},
       {"name":"6890 - Annen kontorkostnad","value":"6890"},
       {"name":"6900 - Telefon","value":"6900"},
       {"name":"6910 - Internett","value":"6910"},
       {"name":"7100 - Kostnad for reise, diett, bil o.l.","value":"7100"},
       {"name":"7140 - Reisekostnad, ikke oppgavepliktig","value":"7140"},
       {"name":"7160 - Diettkostnad, ikke oppgavepliktig","value":"7160"},
       {"name":"7180 - Tannlege, lege etc.","value":"7180"},
       {"name":"7350 - Repesentasjon, fradragsberettiget","value":"7350"}],
       "bilag":"3272",
       "bilag-dx":"2018-12-31",
       "last-date":"2019-9-19",
       "items":
           [{"bilag":"3271","date":"2018-12-31","debit":"7160","credit":"1902","text":"Lunsj","amount":"51.0"},
           {"bilag":"3270","date":"2018-12-27","debit":"6800","credit":"1902","text":"DVD/+R","amount":"56.0"},
           {"bilag":"3270","date":"2018-12-27","debit":"2711","credit":"1902","text":"DVD/+R","amount":"14.0"},
           {"bilag":"3269","date":"2018-12-27","debit":"6340","credit":"1902","text":"Hafslund","amount":"206.7"},
           {"bilag":"3268","date":"2018-12-27","debit":"6800","credit":"1902","text":"Panasonic DVD/BLU, kabler","amount":"1224.0"}]}
-}
