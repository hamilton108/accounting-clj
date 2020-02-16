module Common.Html exposing
    ( Checked(..)
    , HtmlClass(..)
    , HtmlId(..)
    , InputCaption(..)
    , InputType(..)
    , InputValue(..)
    , httpErr2str
    , inputItem
    , labelCheckBox
    , labelInputItem
    , makeInput
    , onChange
    )

import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as JD
import VirtualDom as VD


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


onChange : (String -> a) -> H.Attribute a
onChange tagger =
    E.on "change" (JD.map tagger E.targetValue)


type InputType
    = InputType String


type InputValue
    = InputValue String


type InputCaption
    = InputCaption String


type HtmlClass
    = HtmlClass String


type Checked
    = Checked Bool


type HtmlId
    = HtmlId String


inputItem : InputType -> InputValue -> HtmlClass -> Maybe (String -> msg) -> H.Html msg
inputItem (InputType inputType) (InputValue value) (HtmlClass clazz) event =
    case event of
        Nothing ->
            H.input [ A.value value, A.type_ inputType, A.class clazz ] []

        Just event_ ->
            H.input [ A.value value, E.onInput event_, A.type_ inputType, A.class clazz ] []


labelInputItem : InputCaption -> InputType -> InputValue -> HtmlClass -> Maybe (String -> msg) -> H.Html msg
labelInputItem (InputCaption caption) inputType inputValue htmlClass event =
    H.div
        [ A.class "form-group" ]
        [ H.label [ A.class "control-label" ] [ H.text caption ]
        , inputItem inputType inputValue htmlClass event
        ]


labelCheckBox : HtmlId -> InputCaption -> Checked -> msg -> H.Html msg
labelCheckBox (HtmlId htmlId) (InputCaption caption) (Checked isChecked) event =
    H.div [ A.class "form-group form-group--elm" ]
        [ H.div [ A.class "checkbox" ]
            [ H.label [ A.for htmlId ]
                [ H.input [ A.type_ "checkbox", A.class "fake-cb", A.checked isChecked, E.onClick event, A.id htmlId ] []
                , H.span [ A.class "fake-input" ] []
                , H.span [ A.class "fake-label" ] [ H.text caption ]
                ]
            ]
        ]


makeInput : String -> (String -> msg) -> String -> H.Html msg
makeInput caption msg initVal =
    H.span
        [ A.class "form-group" ]
        [ H.label [] [ H.text caption ]
        , H.input
            [ onChange msg
            , A.class "form-control"
            , A.value initVal
            ]
            []
        ]
