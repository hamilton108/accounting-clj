module GeneralJournal.Types exposing (Flags, Model, Msg(..))

import Browser
import Url exposing (Url)


type alias Flags =
    Int


type Page
    = GalleryPage Gallery.Model
    | FoldersPage Folders.Model
    | NotFound


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotGalleryMsg -- Gallery.Msg



-- | GotFoldersMsg -- Folders.Msg


type alias Model =
    {}
