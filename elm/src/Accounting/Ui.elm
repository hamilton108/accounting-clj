module Accounting.Ui exposing
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
    , timeInput
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


formGroup : LabelText -> H.Html msg -> H.Html msg
formGroup (LabelText labelText) myInput =
    let
        myLabel =
            H.label [] [ H.text labelText ]
    in
    H.div [ A.class "form-group" ]
        [ myLabel
        , myInput
        ]


input : (String -> msg) -> InputType -> LabelText -> Maybe String -> Html msg
input event (InputType inputType) labelText inputValue =
    let
        myVal =
            case inputValue of
                Nothing ->
                    ""

                Just val ->
                    val

        myInput =
            H.input [ A.type_ inputType, A.class "form-control", E.onInput event, A.value myVal ] []
    in
    formGroup labelText myInput


textInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
textInput event labelText inputValue =
    input event (InputType "text") labelText inputValue


numberInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
numberInput event labelText inputValue =
    input event (InputType "number") labelText inputValue


dateInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
dateInput event labelText inputValue =
    input event (InputType "date") labelText inputValue


timeInput : (String -> msg) -> LabelText -> Maybe String -> Html msg
timeInput event labelText inputValue =
    let
        myVal =
            case inputValue of
                Nothing ->
                    ""

                Just val ->
                    val

        myInput =
            H.input [ A.type_ "time", A.step "300", A.class "form-control", E.onInput event, A.value myVal ] []
    in
    formGroup labelText myInput


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



{-
   bootstrapSelect : (String -> msg) -> String -> List (Item msg) -> H.Html msg
   bootstrapSelect event caption items =
       H.span [ A.class "form-group" ]
           [ H.label []
               [ H.text caption ]
           , Select.select
               [ Select.id "myselect"
               , Select.onChange event
               ]
               items
           ]


   bootstrapEmptySelect : Item msg
   bootstrapEmptySelect =
       Select.item [ A.value "" ] [ H.text "-" ]


   type BootstrapInputType
       = Text
       | Date


   type alias BootstrapFn msg =
       List (Option msg) -> H.Html msg


   bootstrapInput : String -> BootstrapFn msg -> List (Option msg) -> H.Html msg
   bootstrapInput caption fn opts =
       H.span [ A.class "form-group" ]
           [ H.label []
               [ H.text caption ]
           , fn opts
           ]


   bootstrapDate : String -> H.Html msg
   bootstrapDate caption =
       bootstrapInput caption Input.date []


   bootstrapText : String -> H.Html msg
   bootstrapText caption =
       bootstrapInput caption Input.text []


   bootstrapTime : String -> H.Html msg
   bootstrapTime caption =
       bootstrapInput caption Input.time []


   bootstrapNumber : String -> H.Html msg
   bootstrapNumber caption =
       bootstrapInput caption Input.number [ Input.attrs [ A.step "300" ] ]

-}


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
            "btn btn-outline-success elm-btn"

        Danger ->
            "btn btn-outline-danger elm-btn"

        DlgSuccess ->
            "btn btn-outline-success btn-modal-dlg elm-btn"

        DlgDanger ->
            "btn btn-outline-danger btn-modal-dlg elm-btn"

        Header ->
            "btn main-btn btn-outline-success elm-btn"

        HeaderDanger ->
            "btn main-btn btn-outline-danger elm-btn"


button :
    msg
    -> BootstrapButton
    -> String
    -> Bool
    -> VD.Node msg
button clickEvent b caption isEnabled =
    H.button [ A.class (buttonClass b), E.onClick clickEvent, A.type_ "button", A.disabled (not isEnabled) ] [ H.text caption ]
