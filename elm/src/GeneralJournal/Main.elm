module GeneralJournal.Main exposing (main)

import Browser
import Browser.Navigation as Nav
import GeneralJournal.Types
    exposing
        ( Flags
        , Model
        , Msg(..)
        )
import GeneralJournal.Update exposing (update)
import GeneralJournal.View exposing (view)
import Url exposing (Url)



--import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Flags =
    Int


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    ( {}, Cmd.none )
