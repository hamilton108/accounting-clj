module Accounting.Hourlist where

import Prelude

import Halogen as H
import DOM.HTML.Indexed.InputType ( InputType(..) )
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH

import Accounting.UI as UI
import Accounting.UI ( SelectItem
                     , GridPosition(..)
                     , Title(..)
                     )

type State = 
  { msg :: String
  , invoices :: Array SelectItem
  }

data Action
  = SelectChange String
  | CbChange Boolean


component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { msg: "NA", invoices: [ {val: "0", tx: "-"}
                                                  , {val: "1", tx: "one"}
                                                  , {val: "2", tx: "two"}
                                                  ] }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }


render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let 
    inv = UI.gridItem (GridPosition "a1") (UI.mkSelect (Title "Faktura") state.invoices SelectChange)
    txt = UI.gridItem (GridPosition "a2") (UI.mkInput (Title "Msg") InputText SelectChange)
    cb = UI.gridItem (GridPosition "b2") (UI.mkCheckbox (Title "Demo") CbChange)
    msg = UI.gridItem (GridPosition "b1") (HH.p_ [ HH.text state.msg ])
  in
  HH.div 
    [ HP.classes [ HH.ClassName "accounting-grid" ]]
    [ inv 
    , txt
    , msg
    , cb
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  (SelectChange s) -> H.modify_ \st -> st { msg = s }
  (CbChange b) -> H.modify_ \st -> st { msg = if b == true then "true"
                                              else "false" }