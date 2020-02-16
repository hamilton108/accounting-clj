module Common.Buttons exposing (..)

import VirtualDom as VD
import Html as H
import Html.Attributes as A
import Html.Events as E


button :
    String
    -> a
    -> VD.Node a
button caption clickEvent =
    H.div [ A.class "form-group form-group--elm" ]
        [ H.button [ A.class "btn btn-outline-success", E.onClick clickEvent ] [ H.text caption ]
        ]
