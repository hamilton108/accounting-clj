module Accounting.AccountingError where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Accounting.Common (alert)

data AccountingError = 
  AffjaxError String
  | JsonError String


handleErrorAff :: AccountingError -> Aff Unit
handleErrorAff (AffjaxError err) = 
  liftEffect $ alert $ "AffjaxError: " <> err 
handleErrorAff (JsonError err) = 
  liftEffect $ alert $ "JsonError: " <> err 


errToString :: AccountingError -> String
errToString (AffjaxError err) = 
  "AffjaxError: " <> err 
errToString (JsonError err) = 
  "JsonError: " <> err 
