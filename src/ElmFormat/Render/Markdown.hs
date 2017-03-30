module ElmFormat.Render.Markdown where

import Cheapskate.Types
import Data.Foldable (fold, toList)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Elm.Utils ((|>))


formatMarkdown :: (String -> Maybe String) -> Blocks -> String
formatMarkdown formatCode blocks =
    let
        blocks' =
            toList blocks

        needsInitialBlanks =
            case blocks' of
                (Para _ : _) -> False
                [] -> False
                _ -> True

        needsTrailingBlanks =
            case blocks' of
                [] -> False
                (_ : []) -> False
                _ -> True
    in
        formatMarkdown' formatCode False needsInitialBlanks needsTrailingBlanks blocks'


formatMarkdown' :: (String -> Maybe String) -> Bool -> Bool -> Bool -> [Block] -> String
formatMarkdown' formatCode isListItem needsInitialBlanks needsTrailingBlanks blocks =
    let
        intersperse =
            case (isListItem, blocks) of
                (True, [Para _, List _ _ _]) -> id
                _ -> List.intersperse "\n"
    in
    (if needsInitialBlanks then "\n\n" else "")
        ++ (fold $ intersperse $ fmap (formatMardownBlock formatCode) $ blocks)
        ++ (if needsTrailingBlanks then "\n" else "")


formatMardownBlock :: (String -> Maybe String) -> Block -> String
formatMardownBlock formatCode block =
    case block of
        Para inlines ->
            (fold $ fmap formatMarkdownInline $ inlines) ++ "\n"

        Header level inlines ->
            "\n" ++ replicate level '#' ++ " " ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "\n"

        Blockquote blocks ->
            formatMarkdown' formatCode False False False (toList blocks)
                |> prefix' "> " "> "

        List tight (Bullet _) items ->
            fold $ (if tight then id else List.intersperse "\n") $
                fmap (prefix' "  - " "    " . formatMarkdown' formatCode True False False . toList) items
        List tight (Numbered _ _) items ->
            fold $ (if tight then id else List.intersperse "\n") $
                fmap (formatListItem formatCode) $ zip [1..] items

        CodeBlock (CodeAttr lang info) code ->
            let
                formatted =
                    fromMaybe (Text.unpack code) $ formatCode $ Text.unpack code

                lang' =
                    Text.unpack lang
            in
                if lang' == "elm" || lang' == ""
                    then unlines $ fmap ((++) "    ") $ lines $ formatted
                    else "```" ++ Text.unpack lang ++ "\n" ++ formatted ++ "\n```\n"

        HtmlBlock text ->
            Text.unpack text ++ "\n"

        HRule ->
            "---\n"

        ReferencesBlock refs ->
            fold $ fmap formatRef refs


formatListItem :: (String -> Maybe String) -> (Int, Blocks) -> String
formatListItem formatCode (i, item)=
    let
        pref =
            if i < 10
                then show i ++ ".  "
                else show i ++ ". "
    in
        prefix' pref "    " $ formatMarkdown' formatCode True False False (toList item)


formatRef :: (Text, Text, Text) -> String
formatRef (label, url, title) =
    "[" ++ Text.unpack label ++ "]: " ++ Text.unpack url
      ++ (if Text.unpack title == "" then "" else " \"" ++ Text.unpack title ++ "\"")
      ++ "\n"


prefix' :: String -> String -> String -> String
prefix' preFirst preRest =
    unlines . prefix preFirst preRest . lines


prefix :: [a] -> [a] -> [[a]] -> [[a]]
prefix _ _ [] = []
prefix preFirst preRest (first:rest) =
    (preFirst ++ first) : fmap ((++) preRest) rest


formatMarkdownInline :: Inline -> String
formatMarkdownInline inline =
    case inline of
        Str text ->
            Text.unpack text
        Space ->
            " "
        SoftBreak ->
            "\n"
        LineBreak ->
            "\n"
        Emph inlines ->
            "*" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "*" -- TODO: escaping
        Strong inlines ->
            "**" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "**" -- TODO: escaping
        Code text ->
            "`" ++ Text.unpack text ++ "`" -- TODO: escape backticks

        Link inlines (Url url) title ->
            let
                text = fold $ fmap formatMarkdownInline $ inlines

                title' = Text.unpack title
                url' = Text.unpack url
            in
                if text == url' && title' == ""
                    then
                        "<" ++ url' ++ ">"
                    else
                        "[" ++ text
                            ++ "](" ++ Text.unpack url
                            ++ (if title' == "" then "" else " \"" ++ title' ++ "\"")
                            ++ ")"

        Link inlines (Ref ref) _ ->
            let
                text = fold $ fmap formatMarkdownInline $ inlines

                ref' = Text.unpack ref
            in
                if text == ref' || ref' == ""
                    then "[" ++ text ++ "]"
                    else "[" ++ text ++ "][" ++ ref' ++ "]"

        Image inlines url title ->
            "![" ++ (fold $ fmap formatMarkdownInline $ inlines)
                ++ "](" ++ Text.unpack url
                ++ (if Text.unpack title == "" then "" else " \"" ++ Text.unpack title ++ "\"")
                ++ ")"

        Entity text ->
            Text.unpack text
        RawHtml text ->
            Text.unpack text
