module Accounting.Common where
  
import Prelude

import Effect (Effect)

foreign import alert :: String -> Effect Unit
