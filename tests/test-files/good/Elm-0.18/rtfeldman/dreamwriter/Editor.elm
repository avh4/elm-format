module Component.Editor exposing (Addresses, Model, initialModel, lazyViewChapter, pluralize, view, viewChapterBody, viewChapterHeading, viewEditor, viewEditorFooter, viewEditorHeader, viewFontControl, viewFullscreenButton, viewOutline, withCommas)

import Component.WordGraph as WordGraph
import Dreamwriter exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Encode exposing (string)
import List exposing (..)
import Maybe
import Signal exposing (Address, mailbox)
import String


type alias Addresses a =
    { a
        | fullscreen : Address FullscreenState
        , remoteSync : Address ()
        , execCommand : Address String
    }


type alias Model =
    { currentDoc : Doc
    , fullscreen : FullscreenState
    }


initialModel : Model
initialModel =
    { currentDoc = emptyDoc
    , fullscreen = False
    }


view : Addresses a -> Model -> Html
view channels model =
    lazy3 viewEditor channels model.currentDoc model.fullscreen


viewEditor : Addresses a -> Doc -> FullscreenState -> Html
viewEditor channels currentDoc fullscreen =
    div [ id "editor-container" ]
        [ div [ id "editor-frame" ]
            [ viewEditorHeader channels currentDoc fullscreen
            , viewOutline channels currentDoc fullscreen
            , viewEditorFooter channels currentDoc fullscreen
            ]
        ]


viewEditorHeader : Addresses a -> Doc -> FullscreenState -> Html
viewEditorHeader channels currentDoc fullscreen =
    div [ id "editor-header" ]
        [ div [ class "toolbar-section toolbar-button flaticon-zoom19" ] []
        , div [ class "toolbar-section" ]
            [ viewFontControl channels.execCommand
                "toggle-bold"
                "B"
                "bold"
            , viewFontControl channels.execCommand
                "toggle-italics"
                "I"
                "italic"
            , viewFontControl channels.execCommand
                "toggle-strikethrough"
                " S "
                "strikethrough"
            ]
        , lazy2 viewFullscreenButton channels.fullscreen fullscreen
        ]


viewEditorFooter : Addresses a -> Doc -> FullscreenState -> Html
viewEditorFooter channels currentDoc fullscreen =
    let
        countChapterWords chapter =
            chapter.headingWords + chapter.bodyWords

        chapterWords =
            currentDoc.chapters
                |> List.map countChapterWords
                |> List.sum

        wordCount =
            currentDoc.titleWords
                + currentDoc.descriptionWords
                + chapterWords

        wordCountLabel =
            pluralize "word" wordCount ++ " saved "
    in
    div [ id "editor-footer" ]
        [ div [ id "doc-word-count" ]
            [ text wordCountLabel
            , WordGraph.viewWordGraph currentDoc.dailyWords
            ]
        , div [ id "dropbox-sync" ]
            [ input
                [ id "toggle-dropbox-sync"
                , property "type" (string "checkbox")
                , onClick channels.remoteSync ()
                ]
                []
            , label [ for "toggle-dropbox-sync" ]
                [ text " sync to Dropbox" ]
            ]
        ]


viewOutline : Addresses a -> Doc -> FullscreenState -> Html
viewOutline channels currentDoc fullscreen =
    let
        outlineHeadingNodes =
            [ h1 [ id "edit-title" ] []
            , div [ id "edit-description" ] []
            ]

        outlineChapterNodes =
            List.concatMap (.id >> lazyViewChapter)
                currentDoc.chapters
    in
    div [ id "document-page" ]
        (outlineHeadingNodes ++ outlineChapterNodes)


withCommas : Int -> String
withCommas num =
    if num >= 1000 then
        let
            prefix =
                (num / 1000)
                    |> floor
                    |> withCommas

            suffix =
                num
                    |> toString
                    |> String.right 3
        in
        prefix ++ "," ++ suffix

    else
        toString num


pluralize : String -> Int -> String
pluralize noun quantity =
    if quantity == 1 then
        "1 " ++ noun

    else
        withCommas quantity ++ " " ++ noun ++ "s"


viewFullscreenButton : Address FullscreenState -> FullscreenState -> Html
viewFullscreenButton fullscreenChannel fullscreen =
    let
        { fullscreenClass, targetMode, fullscreenTitle } =
            case fullscreen of
                True ->
                    { fullscreenClass = "flaticon-collapsing"
                    , targetMode = False
                    , fullscreenTitle = "Leave Fullscreen Mode"
                    }

                False ->
                    { fullscreenClass = "flaticon-expand"
                    , targetMode = True
                    , fullscreenTitle = "Enter Fullscreen Mode"
                    }
    in
    div
        [ class ("toolbar-section toolbar-button " ++ fullscreenClass)
        , title fullscreenTitle
        , onClick fullscreenChannel targetMode
        ]
        []


lazyViewChapter : Identifier -> List Html
lazyViewChapter chapterId =
    [ lazy viewChapterHeading chapterId
    , lazy viewChapterBody chapterId
    ]


viewChapterBody : Identifier -> Html
viewChapterBody chapterId =
    div
        [ key ("chapter-body-" ++ chapterId)
        , id ("edit-chapter-body-" ++ chapterId)
        , class "chapter-body"
        ]
        []


viewChapterHeading : Identifier -> Html
viewChapterHeading chapterId =
    h2
        [ key ("chapter-heading-" ++ chapterId)
        , id ("edit-chapter-heading-" ++ chapterId)
        , class "chapter-heading"
        ]
        []


viewFontControl : Address String -> String -> String -> String -> Html
viewFontControl execCommandChannel idAttr label command =
    span
        [ class "font-control toolbar-button toolbar-font-button"
        , id idAttr
        , property "unselectable" (string "on")
        , onClick execCommandChannel command
        ]
        [ text label ]
