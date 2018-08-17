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
                (Para inlines : _ ) ->
                    case toList inlines of
                        (Str a : Str b : _) ->
                            if (a == Text.pack "@") && (b == Text.pack "docs")
                                then True
                                else False
                        _ -> False
                [] -> False
                _ -> True

        needsTrailingBlanks =
            case blocks' of
                [] -> False
                (_ : []) -> needsInitialBlanks
                _ -> True
    in
        formatMarkdown' formatCode False needsInitialBlanks needsTrailingBlanks blocks'


mapWithPrev :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrev _ [] = []
mapWithPrev f (first:rest) =
    f Nothing first : zipWith (\prev next -> f (Just prev) next) (first:rest) rest


formatMarkdown' :: (String -> Maybe String) -> Bool -> Bool -> Bool -> [Block] -> String
formatMarkdown' formatCode isListItem needsInitialBlanks needsTrailingBlanks blocks =
    let
        intersperse =
            case (isListItem, blocks) of
                (True, [Para _, List _ _ _]) -> id
                _ -> List.intersperse "\n"

        contextFor prev =
            case prev of
                Just (List _ _ _) -> AfterIndentedList
                _ -> Normal
    in
    (if needsInitialBlanks then "\n\n" else "")
        ++ (fold $ intersperse $ mapWithPrev (\prev -> formatMardownBlock formatCode (contextFor prev)) $ blocks)
        ++ (if needsTrailingBlanks then "\n" else "")


data Context
    = Normal
    | AfterIndentedList


formatMardownBlock :: (String -> Maybe String) -> Context -> Block -> String
formatMardownBlock formatCode context block =
    case block of
        ElmDocs terms ->
            (List.intercalate "\n" $ fmap ((++) "@docs " . List.intercalate ", " . fmap Text.unpack) terms) ++ "\n"

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

        CodeBlock (CodeAttr lang _info) code ->
            let
                formatted =
                    fromMaybe (Text.unpack $ ensureNewline code) $ formatCode $ Text.unpack code

                ensureNewline text =
                    if Text.last text == '\n'
                        then text
                        else Text.snoc text '\n'

                lang' =
                    Text.unpack lang

                isElm =
                    lang' == "elm" || lang' == ""

                canIndent =
                    case context of
                        Normal -> True
                        AfterIndentedList -> False
            in
                if isElm && canIndent
                    then unlines $ fmap ((++) "    ") $ lines $ formatted
                    else "```" ++ Text.unpack lang ++ "\n" ++ formatted ++ "```\n"

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
            Text.unpack $ Text.concatMap fix text
        Space ->
            " "
        SoftBreak ->
            "\n"
        LineBreak ->
            "\n"
        Emph inlines ->
            "_" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "_" -- TODO: escaping
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
    where
        fix c =
            case c of
                '\\' -> Text.pack "\\\\"
                -- TODO: only at the beginning of words
                '`' -> Text.pack "\\`"
                '_' -> Text.pack "\\_"
                '*' -> Text.pack "\\*"
                -- TODO: {}  curly braces (when?)
                -- TODO: []  square brackets (when?)
                -- TODO: ()  parentheses (when?)
                -- TODO: #   hash mark (only at the beginning of lines, and within header lines?)
                -- TODO: -   minus sign (hyphen) (only at the beginning of lines?)
                -- TODO: +   plus sign (when?)
                -- TODO: .   dot (when?)
                -- TODO: !   exclamation mark (when?)
                _ -> Text.singleton c
