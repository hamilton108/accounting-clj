module Accounting.Main exposing (main)

import Accounting.Types
    exposing
        ( Flags
        , Model
        , Msg(..)
        , Page(..)
        , Route(..)
        )
import Accounting.Update exposing (update, updateUrl)
import Accounting.View exposing (view)
import Browser
import Browser.Navigation as Nav
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
init flags url key =
    updateUrl url { page = NotFound, key = key }
