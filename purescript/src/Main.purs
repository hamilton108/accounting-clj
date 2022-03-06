module Main where

import Prelude

import Accounting.Hourlist as Hourlist
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))
import Effect.Console (logShow)
import Effect.Class (liftEffect)

main :: Effect Unit
main = HA.runHalogenAff $
    HA.awaitLoad *> 
    HA.selectElement (QuerySelector "#ps-hourlist") >>= \node ->
        case node of 
            Nothing ->
                liftEffect (logShow "No such element") 
            Just nodex ->
                runUI Hourlist.component unit nodex *>
                pure unit 
