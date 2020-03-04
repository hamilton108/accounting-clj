module Accounting.GeneralJournal exposing (Model, Msg(..), init, update, view)

import Accounting.Ui
    exposing
        ( GridPosition(..)
        , LabelText(..)
        , SelectItem
        , SelectItems
        , dateInput
        , gridItem
        , makeSelect
        , numberInput
        , textInput
        )
import Html as H
import Html.Attributes as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP


mainUrl : String
mainUrl =
    "/generaljournal"


initUrl : String
initUrl =
    mainUrl ++ "/latestdata"


type Date
    = Date String


type Debit
    = Debit String


type Desc
    = Desc String


type Bilag
    = Bilag String


type Belop
    = Belop String


type MvaAmount
    = MvaAmount String


type Mva
    = Mva String


type alias Model =
    { ns4102 : SelectItems
    , lastBilagDate : Date
    , bilag : Bilag
    , date : Maybe Date
    , debit : Maybe Debit
    , desc : Maybe Desc
    , belop : Maybe Belop
    , mvaAmount : Maybe MvaAmount
    , mva : Maybe Mva
    , selectedNs4102 : Maybe String
    }


type Msg
    = Noop
    | DateChanged String
    | DebitChanged String
    | DescChanged String
    | BilagChanged String
    | BelopChanged String
    | MvaAmountChanged String
    | MvaChanged String
    | InitDataFetched (Result Http.Error Model)


init : ( Model, Cmd Msg )
init =
    ( { ns4102 = []
      , lastBilagDate = Date ""
      , bilag = Bilag "0"
      , date = Nothing
      , debit = Nothing
      , desc = Nothing
      , belop = Nothing
      , mvaAmount = Nothing
      , mva = Nothing
      , selectedNs4102 = Nothing
      }
    , fetchInitData
    )


view : Model -> H.Html Msg
view model =
    let
        curdate =
            dateInput DateChanged (LabelText "Dato") Nothing

        debet =
            makeSelect DebitChanged "Debet" model.ns4102 Nothing

        desc =
            textInput DescChanged (LabelText "Tekst") Nothing

        bilag =
            let
                (Bilag curBilag) =
                    model.bilag
            in
            numberInput BilagChanged (LabelText "Bilag") (Just curBilag)

        belop =
            numberInput BelopChanged (LabelText "Beløp") Nothing

        mva_amount =
            numberInput MvaChanged (LabelText "Mva beløp") (Just "0.0")
    in
    H.div [ A.class "accounting-grid" ]
        [ gridItem (GridPosition "a1") curdate
        , gridItem (GridPosition "b1") debet
        , gridItem (GridPosition "c1") desc
        , gridItem (GridPosition "d1") bilag
        , gridItem (GridPosition "e1") belop
        , gridItem (GridPosition "a2") mva_amount
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        DebitChanged s ->
            let
                curNs4102 =
                    if s == "" then
                        Nothing

                    else
                        Just s
            in
            Debug.log "DebitChanged"
                ( { model | selectedNs4102 = curNs4102 }, Cmd.none )

        DateChanged s ->
            Debug.log "Edit Changed"
                ( { model | date = Just (Date s) }, Cmd.none )

        DescChanged s ->
            Debug.log "Edit Changed"
                ( model, Cmd.none )

        BilagChanged s ->
            ( { model | bilag = Bilag s }, Cmd.none )

        BelopChanged s ->
            Debug.log "Edit Changed"
                ( model, Cmd.none )

        MvaAmountChanged s ->
            Debug.log "Edit Changed"
                ( model, Cmd.none )

        MvaChanged s ->
            Debug.log "Edit Changed"
                ( model, Cmd.none )

        InitDataFetched (Ok initData) ->
            ( initData, Cmd.none )

        InitDataFetched (Err err) ->
            Debug.log (httpErr2str err)
                ( model, Cmd.none )


selectItemDecoder : JD.Decoder SelectItem
selectItemDecoder =
    JD.map2 SelectItem
        (JD.field "v" JD.string)
        (JD.field "t" JD.string)


bilagDecoder : JD.Decoder Bilag
bilagDecoder =
    JD.map Bilag
        JD.string



--(JD.field "bilag" JD.string)


lastBilagDateDecoder : JD.Decoder Date
lastBilagDateDecoder =
    JD.map Date
        JD.string



--(JD.field "bilag-dx" JD.string)


initDataDecoder : JD.Decoder Model
initDataDecoder =
    JD.succeed Model
        |> JP.required "ns4102" (JD.list selectItemDecoder)
        |> JP.required "bilag-dx" lastBilagDateDecoder
        |> JP.required "bilag" bilagDecoder
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing
        |> JP.hardcoded Nothing


fetchInitData : Cmd Msg
fetchInitData =
    Debug.log "fetchInitData"
        Http.send
        InitDataFetched
    <|
        Http.get initUrl initDataDecoder



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
