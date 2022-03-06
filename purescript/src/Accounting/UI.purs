module Accounting.UI where

import Prelude
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen.HTML ( HTML
                    , ClassName(..)
                    )
import Halogen.HTML.Events as HE
import DOM.HTML.Indexed.InputType ( InputType(..) )
import Web.UIEvent.MouseEvent as ME


newtype GridPosition = 
  GridPosition String

newtype Title = 
  Title String

type SelectItem = 
  { val :: String
  , tx :: String 
  }

gridItem :: forall w i. GridPosition -> HTML w i -> HTML w i
gridItem (GridPosition clazz) item =
  HH.div [ HP.classes [ ClassName clazz ]] [item ]

mkOption :: forall w i. SelectItem -> HTML w i
mkOption item = 
  HH.option 
    [ HP.value item.val ] 
    [ HH.text item.tx ]

mkSelect_ :: forall w i. Array SelectItem -> (String -> i) -> HTML w i
mkSelect_ items evt = 
  let 
    opts = map mkOption items
  in
  HH.select
    [ HP.classes [ ClassName "form-control" ]
    , HE.onValueChange evt
    ]
    opts

mkSelect :: forall w i. Title -> Array SelectItem -> (String -> i) -> HTML w i
mkSelect (Title title) items evt = 
  let 
    sel = mkSelect_ items evt
  in
  HH.span [ HP.classes [ ClassName "form-group" ]]
    [ HH.label [] [ HH.text title ]
    , sel
    ]

mkInput_ :: forall w i. InputType -> (String -> i) -> HTML w i
mkInput_ inpType evt = 
  HH.input 
    [ HP.type_ inpType --IP.InputText
    , HP.classes 
      [ ClassName "form-control"
      ]
    , HE.onValueChange evt
    ]

mkInput :: forall w i. Title -> InputType -> (String -> i) -> HTML w i
mkInput (Title title) inpType evt = 
  let 
    inp = mkInput_ inpType evt
  in
  HH.span [ HP.classes [ ClassName "form-group" ]]
    [ HH.label [] [ HH.text title ]
    , inp
    ]

mkCheckbox :: forall w i. Title -> (Boolean -> i) -> HTML w i 
mkCheckbox (Title title) evt = 
  HH.div 
    [ HP.classes 
      [ ClassName "form-group"
      ]
    ]
    [ HH.div [ HP.classes [ ClassName "checkbox" ]]
      [ HH.label_
        [ HH.input
          [ HP.type_ InputCheckbox
          , HP.classes [ ClassName "fake-cb" ]
          , HE.onChecked evt
          ]
        , HH.span [ HP.classes [ ClassName "fake-input" ] ]
          []
        , HH.span [ HP.classes [ ClassName "fake-label" ] ]
          [ HH.text title
          ]
        ]
      ]
    ]


mkButton :: forall w i. Title -> (ME.MouseEvent -> i) -> HTML w i
mkButton (Title title) evt = 
  HH.button
    [ HE.onClick evt
    , HP.classes 
      [ ClassName "btn"
      , ClassName "btn-outline-success" 
      ]
    ]
    [ HH.text title ]