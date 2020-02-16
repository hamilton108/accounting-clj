module GeneralJournal.Types exposing (Flags, Model, Msg(..))

import Browser
import Url exposing (Url)


type alias Flags =
    Int


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotGalleryMsg -- Gallery.Msg



-- | GotFoldersMsg -- Folders.Msg


type alias Model =
    {}
