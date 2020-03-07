module Accounting.Ui exposing
    ( BootstrapButton(..)
    , GridPosition(..)
    , LabelText(..)
    , SelectItem
    , SelectItems
    , button
    , checkBoxInput
    , dateInput
    , gridItem
    , makeSelect
    , numberInput
    , textInput
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import VirtualDom as VD


onChange : (String -> a) -> H.Attribute a
onChange tagger =
    E.on "change" (JD.map tagger E.targetValue)


type InputType
    = InputType String


type LabelText
    = LabelText String


type GridPosition
    = GridPosition String


gridItem : GridPosition -> Html msg -> Html msg
gridItem (GridPosition clazz) item =
    H.div [ A.class clazz ] [ item ]



{-
   <div class="form-group">
     <label for="exampleInputEmail1">Email address</label>
     <input type="email" class="form-control" id="exampleInputEmail1" aria-describedby="emailHelp" placeholder="Enter email">
     <small id="emailHelp" class="form-text text-muted">We'll never share your email with anyone else.</small>
   </div>
-}


input : (String -> msg) -> InputType -> LabelText -> Maybe String -> Html msg
input event (InputType inputType) (LabelText labelText) inputValue =
    let
        myLabel =
            H.label [] [ H.text labelText ]

        myVal =
            case inputValue of
                Nothing ->
                    ""

                Just val ->
                    val

        myInput =
            H.input [ A.type_ inputType, A.class "form-control", E.onInput event, A.value myVal ] []
    in
    H.div [ A.class "form-group" ]
        [ myLabel
        , myInput
        ]


toString : Bool -> String
toString bool =
    if bool then
        "True"

    else
        "False"


checkBoxInput : msg -> LabelText -> Bool -> Html msg
checkBoxInput event (LabelText label) isChecked =
    let
        myInput =
            if isChecked == True then
                H.input [ E.onClick event, A.id label, A.type_ "checkbox", A.class "form-check-input", A.attribute "checked" "checked" ] []

            else
                H.input [ E.onClick event, A.id label, A.type_ "checkbox", A.class "form-check-input" ] []
    in
    Debug.log (toString isChecked)
        H.div
        [ A.class "form-check form-check-inline" ]
        [ myInput
        , H.label [ A.attribute "for" label, A.class "form-check-label" ] [ H.text label ]
        ]



{-
   checkBoxInput : (String -> msg) -> LabelText -> GridPosition -> Bool -> Html msg
   checkBoxInput event (LabelText label) (GridPosition gridPos) isChecked =
       let
           myInput =
               if isChecked == True then
                   H.input [ E.onClick event, A.id label, A.type_ "checkbox", A.class "form-check-input", A.attribute "checked" "checked" ] []

               else
                   H.input [ E.onClick event, A.id label, A.type_ "checkbox", A.class "form-check-input" ] []
       in
       H.div [ A.class gridPos ]
           [ H.div [ A.class "form-check form-check-inline" ]
               [ myInput
               , H.label [ A.attribute "for" label, A.class "form-check-label" ] [ H.text label ]
               ]
           ]
-}


textInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
textInput event labelText inputValue =
    input event (InputType "text") labelText inputValue


numberInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
numberInput event labelText inputValue =
    input event (InputType "number") labelText inputValue


dateInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
dateInput event labelText inputValue =
    input event (InputType "date") labelText inputValue


type alias SelectItem =
    { val : String
    , txt : String
    }


type alias SelectItems =
    List SelectItem


emptySelectOption : VD.Node a
emptySelectOption =
    H.option
        [ A.value ""
        ]
        [ H.text "-" ]


makeSelectOption : Maybe String -> SelectItem -> VD.Node a
makeSelectOption selected item =
    case selected of
        Nothing ->
            H.option
                [ A.value item.val
                ]
                [ H.text item.txt ]

        Just sel ->
            H.option
                [ A.value item.val
                , A.selected (sel == item.val)
                ]
                [ H.text item.txt ]


makeSelect : (String -> a) -> String -> SelectItems -> Maybe String -> VD.Node a
makeSelect event caption payload selected =
    let
        makeSelectOption_ =
            makeSelectOption selected

        px =
            emptySelectOption :: List.map makeSelectOption_ payload
    in
    H.span [ A.class "form-group" ]
        [ H.label [] [ H.text caption ]
        , H.select
            [ onChange event
            , A.class "form-control"
            ]
            px
        ]


type BootstrapButton
    = Success
    | Danger
    | DlgSuccess
    | DlgDanger
    | Header
    | HeaderDanger


buttonClass : BootstrapButton -> String
buttonClass b =
    case b of
        Success ->
            "btn btn-outline-success"

        Danger ->
            "btn btn-outline-danger"

        DlgSuccess ->
            "btn btn-outline-success btn-modal-dlg"

        DlgDanger ->
            "btn btn-outline-danger btn-modal-dlg"

        Header ->
            "btn main-btn btn-outline-success"

        HeaderDanger ->
            "btn main-btn btn-outline-danger"


button :
    a
    -> BootstrapButton
    -> String
    -> Bool
    -> VD.Node a
button clickEvent b caption isEnabled =
    H.button [ A.class (buttonClass b), E.onClick clickEvent, A.type_ "button", A.disabled (not isEnabled) ] [ H.text caption ]
