module Component.LeftSidebar exposing (Addresses, Model, Update(..), ViewMode(..), downloadContentType, illegalFilenameCharMatcher, initialModel, legalizeFilename, sidebarHeaderClass, sidebarHeaderId, transition, view, viewCurrentDocFooter, viewCurrentDocHeader, viewOpenMenuFooter, viewOpenMenuHeader)

import Component.LeftSidebar.CurrentDocView as CurrentDoc
import Component.LeftSidebar.OpenMenuView as OpenMenu
import Dreamwriter exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Maybe
import Regex
import Signal exposing (Address)


type ViewMode
    = CurrentDocMode
    | OpenMenuMode
    | SettingsMode


type alias Addresses a =
    { a
        | print : Address ()
        , newDoc : Address ()
        , newChapter : Address ()
        , openFromFile : Address ()
        , navigateToTitle : Address ()
        , navigateToChapterId : Address Identifier
        , download : Address DownloadOptions
        , update : Address Update
    }


type alias Model =
    { viewMode : ViewMode
    , docs : List Doc
    , currentDocId : Maybe Identifier
    , currentDoc : Doc
    }


initialModel : Model
initialModel =
    { viewMode = CurrentDocMode
    , docs = []
    , currentDocId = Nothing
    , currentDoc = emptyDoc
    }


type Update
    = NoOp
    | SetViewMode ViewMode
    | OpenDocId Identifier


transition : Update -> Model -> Model
transition update model =
    case update of
        NoOp ->
            model

        SetViewMode mode ->
            { model | viewMode = mode }

        OpenDocId id ->
            { model
                | currentDocId = Just id
                , viewMode = CurrentDocMode
            }


{-| Replace illegal filename characters with underscores
-}
illegalFilenameCharMatcher =
    Regex.regex "[/\\<>?|\":*]"


legalizeFilename : String -> String
legalizeFilename =
    Regex.replace Regex.All illegalFilenameCharMatcher (always "_")


downloadContentType =
    "text/plain;charset=UTF-8"


view : Addresses a -> Model -> Html
view addresses model =
    let
        openDoc =
            Signal.forwardTo addresses.update OpenDocId

        { sidebarHeader, sidebarBody, sidebarFooter } =
            case model.viewMode of
                OpenMenuMode ->
                    { sidebarHeader =
                        lazy viewOpenMenuHeader
                            addresses.update
                    , sidebarBody =
                        lazy2 (OpenMenu.view addresses.openFromFile openDoc)
                            model.docs
                            model.currentDoc
                    , sidebarFooter =
                        viewOpenMenuFooter
                    }

                CurrentDocMode ->
                    { sidebarHeader =
                        lazy2 viewCurrentDocHeader
                            model.currentDoc
                            addresses
                    , sidebarBody =
                        lazy3 CurrentDoc.view
                            addresses.navigateToTitle
                            addresses.navigateToChapterId
                            model.currentDoc
                    , sidebarFooter =
                        lazy viewCurrentDocFooter
                            addresses
                    }

                SettingsMode ->
                    -- TODO make this different than CurrentDocMode
                    { sidebarHeader =
                        lazy2 viewCurrentDocHeader
                            model.currentDoc
                            addresses
                    , sidebarBody =
                        lazy3 CurrentDoc.view
                            addresses.navigateToTitle
                            addresses.navigateToChapterId
                            model.currentDoc
                    , sidebarFooter =
                        lazy viewCurrentDocFooter
                            addresses
                    }
    in
    div [ id "left-sidebar-container", class "sidebar" ]
        [ sidebarHeader
        , div [ id "left-sidebar-body", class "sidebar-body" ]
            [ sidebarBody ]
        , sidebarFooter
        ]


sidebarHeaderId =
    "left-sidebar-header"


sidebarHeaderClass =
    "sidebar-header"


viewOpenMenuFooter : Html
viewOpenMenuFooter =
    span [] []


viewCurrentDocFooter : Addresses a -> Html
viewCurrentDocFooter addresses =
    div [ id "left-sidebar-footer", class "sidebar-footer" ]
        [ span
            [ id "add-chapter"
            , title "Add Chapter"
            , onClick addresses.newChapter ()
            , class "flaticon-plus81"
            ]
            []
        ]


viewOpenMenuHeader updateChannel =
    div
        [ key "open-menu-header"
        , id sidebarHeaderId
        , class sidebarHeaderClass
        ]
        [ span
            [ class "sidebar-header-control"
            , onClick updateChannel (SetViewMode CurrentDocMode)
            ]
            [ text "cancel" ]
        ]


viewCurrentDocHeader : Doc -> Addresses a -> Html
viewCurrentDocHeader currentDoc addresses =
    let
        downloadOptions =
            { filename = legalizeFilename currentDoc.title ++ ".html"
            , contentType = downloadContentType
            }
    in
    menu [ id sidebarHeaderId, class sidebarHeaderClass ]
        [ menuitem
            [ title "New"
            , class "sidebar-header-control flaticon-add26"
            , onClick addresses.newDoc ()
            ]
            []
        , menuitem
            [ title "Open"
            , class "sidebar-header-control flaticon-folder63"
            , onClick addresses.update (SetViewMode OpenMenuMode)
            ]
            []
        , menuitem
            [ title "Download"
            , class "sidebar-header-control flaticon-cloud134"
            , onClick addresses.download downloadOptions
            ]
            []
        , menuitem
            [ title "Print"
            , class "sidebar-header-control flaticon-printer70"
            , onClick addresses.print ()
            ]
            []
        , menuitem
            [ title "Settings"
            , class "sidebar-header-control flaticon-gear33"
            , onClick addresses.update (SetViewMode SettingsMode)
            ]
            []
        ]
