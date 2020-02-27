module Accounting.Types exposing (Flags, Model, Msg(..), Page(..), Route(..))

import Accounting.GeneralJournal as Gj
import Accounting.HourList as HourList
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)


type alias Flags =
    Int


type Page
    = GeneralJournalPage Gj.Model
    | HourListPage HourList.Model
    | NotFound


type Route
    = GeneralJournalRoute
    | HourListRoute


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GeneralJournalMsg Gj.Msg
    | HourListMsg HourList.Msg



-- | GotFoldersMsg -- Folders.Msg


type alias Model =
    { page : Page
    , key : Nav.Key
    }
