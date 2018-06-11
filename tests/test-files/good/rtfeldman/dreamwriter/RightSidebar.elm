module Component.RightSidebar exposing (Addresses, Model, initialModel, onInput, view, viewCurrentNoteBody, viewCurrentNoteFooter, viewHeader, viewNoteListing, viewNoteListings)

import Dreamwriter exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import List exposing (..)
import Maybe
import Signal exposing (Address)


type alias Addresses a =
    { a
        | newNote : Address ()
        , searchNotes : Address String
        , openNoteId : Address Identifier
    }


type alias Model =
    { currentNote : Maybe Note
    , notes : List Note
    }


initialModel : Model
initialModel =
    { currentNote = Nothing
    , notes = []
    }


view : Addresses a -> Model -> Html
view addresses model =
    let
        { sidebarBody, sidebarFooter } =
            case model.currentNote of
                Nothing ->
                    { sidebarBody =
                        lazy2 viewNoteListings
                            addresses.openNoteId
                            model.notes
                    , sidebarFooter =
                        span [] []
                    }

                Just currentNote ->
                    { sidebarBody =
                        lazy viewCurrentNoteBody currentNote
                    , sidebarFooter =
                        lazy2 viewCurrentNoteFooter
                            addresses
                            currentNote
                    }
    in
    div [ id "right-sidebar-container", class "sidebar" ]
        [ div [ id "right-sidebar-body", class "sidebar-body" ]
            [ sidebarBody ]
        , sidebarFooter
        ]


viewHeader : Addresses a -> Model -> Html
viewHeader addresses model =
    div [ id "right-sidebar-header", class "sidebar-header" ]
        [ input
            [ id "notes-search-text"
            , class "sidebar-header-control"
            , placeholder "search notes"
            , onInput addresses.searchNotes targetValue
            ]
            []
        , span
            [ id "notes-search-button"
            , class "sidebar-header-control flaticon-pencil90"
            , onClick addresses.newNote ()
            ]
            []
        ]


viewNoteListings openNoteIdChannel notes =
    div [ id "note-listings" ]
        (List.map (viewNoteListing openNoteIdChannel) notes)


viewNoteListing : Address Identifier -> Note -> Html
viewNoteListing openNoteId note =
    div
        [ key ("note-" ++ note.id)
        , class "note-listing"
        , onClick openNoteId note.id
        ]
        [ div [ class "flaticon-document127 note-listing-icon" ] []
        , div [ class "note-listing-title" ] [ text note.title ]
        ]


viewCurrentNoteBody : Note -> Html
viewCurrentNoteBody note =
    div [ key ("current-note-" ++ note.id), id "current-note" ]
        [ div [ id "current-note-title-container" ]
            [ div [ id "current-note-title" ] [] ]
        , div [ id "current-note-body" ] []
        ]


onInput address decoder =
    on "input" decoder (Signal.message address)


viewCurrentNoteFooter : Addresses a -> Note -> Html
viewCurrentNoteFooter addresses note =
    div [ id "current-note-controls", class "sidebar-footer" ] []



--div [ id "current-note-controls", class "sidebar-footer" ] [
--    span [ id "download-current-note",
--        title "Download Note",
--        class "flaticon-cloud134 current-note-control" ] [],
--    span [ id "print-current-note",
--        title "Print Note",
--        class "flaticon-printer70 current-note-control" ] [],
--    span [ id "current-note-settings",
--        title "Note Settings",
--        class "flaticon-gear33 current-note-control" ] [],
--    span [ id "delete-current-note",
--        title "Delete Note",
--        class "flaticon-closed18 current-note-control" ] []
--]
