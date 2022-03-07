module Accounting.UI where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed.InputType ( InputType(..) )
import Web.UIEvent.MouseEvent ( MouseEvent )
--import Web.Event.Event (Event)

import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen.HTML ( HTML
                    , ClassName(..)
                    )
import Halogen.HTML.Events as HE


newtype GridPosition = 
  GridPosition String

newtype Title = 
  Title String

newtype InputVal = 
  InputVal String

type SelectItem = 
  { v :: String
  , t :: String 
  }

type SelectItems = Array SelectItem

emptySelectItem :: SelectItem
emptySelectItem = { v: "0", t: "-" }

gridItem :: forall w i. GridPosition -> HTML w i -> HTML w i
gridItem (GridPosition clazz) item =
  HH.div [ HP.classes [ ClassName clazz ]] [item ]

{-
defaultEventHandling :: Event.Event -> Effect Unit
defaultEventHandling event = 
    Event.stopPropagation event *>
    Event.preventDefault event 
-}

mkOption :: forall w i. SelectItem -> HTML w i
mkOption item = 
  HH.option 
    [ HP.value item.v ] 
    [ HH.text item.t ]

mkSelect_ :: forall w i. SelectItems -> (String -> i) -> HTML w i
mkSelect_ items evt = 
  let 
    opts = map mkOption (emptySelectItem : items)
  in
  HH.select
    [ HP.classes [ ClassName "form-control" ]
    , HE.onValueChange evt
    ]
    opts

mkSelect :: forall w i. Title -> SelectItems -> (String -> i) -> HTML w i
mkSelect (Title title) items evt = 
  let 
    sel = mkSelect_ items evt
  in
  HH.span [ HP.classes [ ClassName "form-group" ]]
    [ HH.label [] [ HH.text title ]
    , sel
    ]

mkInput_ :: forall w i. InputType -> (String -> i) -> Maybe InputVal -> HTML w i
mkInput_ inpType evt val = 
  case val of
    Nothing ->
      HH.input 
        [ HP.type_ inpType 
        , HP.classes 
          [ ClassName "form-control"
          ]
        , HE.onValueChange evt
        ]
    Just (InputVal val1) -> 
      HH.input 
        [ HP.type_ inpType 
        , HP.classes 
          [ ClassName "form-control"
          ]
        , HP.value val1
        , HE.onValueChange evt
        ]

mkInput :: forall w i. Title -> InputType -> (String -> i) -> Maybe InputVal -> HTML w i
mkInput (Title title) inpType evt val = 
  let 
    inp = mkInput_ inpType evt val
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


mkButton :: forall w i. Title -> (MouseEvent -> i) -> HTML w i
mkButton (Title title) evt = 
  HH.button
    [ HE.onClick evt
    , HP.classes 
      [ ClassName "btn"
      , ClassName "btn-outline-success" 
      ]
    ]
    [ HH.text title ]