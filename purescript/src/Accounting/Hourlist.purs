module Accounting.Hourlist where

import Prelude

import Effect.Aff.Class (class MonadAff)
--import Effect.Aff (launchAff_)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat

import DOM.HTML.Indexed.InputType (InputType(..))
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
--import Web.Event.Event (Event)
import Web.Event.Event as E

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode as Decode
import Data.Argonaut.Decode.Error (JsonDecodeError)

import Data.Either (Either(..))

import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH

import Accounting.UI as UI
import Accounting.UI ( SelectItems
                     , GridPosition(..)
                     , Title(..)
                     )
import Accounting.AccountingError (AccountingError(..))
import Accounting.AccountingError as ERR


mainURL :: String
mainURL =
    "/hourlist"

type InitDataJson = 
  { invoices :: SelectItems
  , hourlistgroups :: SelectItems
  }

type State = 
  { msg :: String
  , invoices :: SelectItems
  , hourlistGroups :: SelectItems
  }

data Action
  = SelectChange String
  | CbChange Boolean
  | FetchInvoices MouseEvent

initDataFromJson :: Json -> Either JsonDecodeError InitDataJson
initDataFromJson = Decode.decodeJson

initURL :: String
initURL =
    mainURL <>  "/latestdata" 

fetchInitData :: forall m. MonadAff m => m (Either AccountingError InitDataJson)
fetchInitData = 
  H.liftAff $
    Affjax.get ResponseFormat.json initURL >>= \res ->
      let 
        result :: Either AccountingError InitDataJson
        result = 
          case res of  
            Left err -> 
              Left $ AffjaxError (Affjax.printError err)
            Right response ->
              let 
                initData = initDataFromJson response.body
              in
              case initData of
                Left err ->
                  Left $ JsonError (show err)
                Right initData1 ->
                  Right initData1 
      in
      pure result

initSelectItems :: SelectItems
initSelectItems =
  [ {v: "0", t: "-"} ]

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { msg: "NA"
                          , invoices: initSelectItems
                          , hourlistGroups: initSelectItems
                          }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }


render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let 
    fetchInvBtn = UI.gridItem (GridPosition "a1") (UI.mkButton (Title "Hent faktura") FetchInvoices)
    msg = UI.gridItem (GridPosition "a2") (HH.p_ [ HH.text state.msg ])
    inv = UI.gridItem (GridPosition "b1") (UI.mkSelect (Title "Faktura") state.invoices SelectChange)
    txt = UI.gridItem (GridPosition "b2") (UI.mkInput (Title "Msg") InputText SelectChange)
    --cb = UI.gridItem (GridPosition "b2") (UI.mkCheckbox (Title "Demo") CbChange)
  in
  HH.div 
    [ HP.classes [ HH.ClassName "accounting-grid" ]]
    [ inv 
    , txt
    , msg
    , fetchInvBtn 
    ]

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  (SelectChange s) -> H.modify_ \st -> st { msg = s }
  (CbChange b) -> H.modify_ \st -> st { msg = if b == true then "true"
                                              else "false" }
  (FetchInvoices event) -> 
    (H.liftEffect $ E.preventDefault (ME.toEvent event)) *>
    fetchInitData >>= \initData ->
      case initData of 
        Left err -> 
          H.modify_ \st -> st { msg = "Fetch invoices FAIL: " <> ERR.errToString err }
        Right result -> 
          H.modify_ \st -> st { msg = "Fetch invoices"
                              , invoices = result.invoices 
                              , hourlistGroups = result.hourlistgroups 
                              }
