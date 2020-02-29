module Accounting.Ui exposing (GridPosition(..), gridItem, textInput)

import Html as H exposing (Html)
import Html.Attributes as A


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


textInput : Html msg
textInput =
    let
        myLabel =
            H.label [] [ H.text "label" ]

        myInput =
            H.input [ A.type_ "number", A.class "form-control", A.value "10.0" ] []
    in
    H.div [ A.class "form-group" ]
        [ myLabel
        , myInput
        ]
