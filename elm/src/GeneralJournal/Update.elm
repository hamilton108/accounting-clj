module GeneralJournal.Update exposing (update)

import GeneralJournal.Types
    exposing
        ( Model
        , Msg(..)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            ( model, Cmd.none )

        {-
           case urlRequest of
               Browser.External href ->
                   ( model, Nav.load href )

               Browser.Internal url ->
                   ( model, Nav.pushUrl model.key (Url.toString url) )
        -}
        ChangedUrl url ->
            ( model, Cmd.none )

        {-
               updateUrl url model

           GotFoldersMsg foldersMsg ->
               case model.page of
                   FoldersPage folders ->
                       toFolders model (Folders.update foldersMsg folders)

                   _ ->
                       ( model, Cmd.none )
        -}
        GotGalleryMsg ->
            ( model, Cmd.none )



{-
   case model.page of
       GalleryPage gallery ->
           toGallery model (Gallery.update galleryMsg gallery)

       _ ->
           ( model, Cmd.none )

-}
