module ElmFormat.Render.Markdown where

import Cheapskate.Types
import qualified Data.Char as Char
import Data.Foldable (fold, toList)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Extra (longestSpanOf, LongestSpanResult(..))
import Elm.Utils ((|>))
import qualified Regex
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Text.Lazy as Lazy


formatMarkdown :: (String -> Maybe B.Builder) -> Blocks -> B.Builder
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
        B.stringUtf8 $
        -- TODO: rewrite the rest of this module to be able to create ByteString.Builders directly without intermediate String/Text allocation
        formatMarkdown' formatCode False needsInitialBlanks needsTrailingBlanks blocks'


mapWithPrev :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrev _ [] = []
mapWithPrev f (first:rest) =
    f Nothing first : zipWith (\prev next -> f (Just prev) next) (first:rest) rest


formatMarkdown' :: (String -> Maybe B.Builder) -> Bool -> Bool -> Bool -> [Block] -> String
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


formatMardownBlock :: (String -> Maybe B.Builder) -> Context -> Block -> String
formatMardownBlock formatCode context block =
    case block of
        ElmDocs terms ->
            (List.intercalate "\n" $ fmap ((++) "@docs " . List.intercalate ", " . fmap Text.unpack) terms) ++ "\n"

        Para inlines ->
            (fold $ fmap (formatMarkdownInline True) $ inlines) ++ "\n"

        Header level inlines ->
            "\n" ++ replicate level '#' ++ " " ++ (fold $ fmap (formatMarkdownInline True) $ inlines) ++ "\n"

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
                isElm =
                    lang' == "elm" || lang' == ""

                formatted =
                    maybe
                        (Text.unpack $ ensureNewline code)
                        (Lazy.unpack . Lazy.decodeUtf8 . B.toLazyByteString)
                        $
                        if isElm
                            then formatCode $ Text.unpack code
                            else Nothing

                ensureNewline text =
                    if Text.last text == '\n'
                        then text
                        else Text.snoc text '\n'

                lang' =
                    Text.unpack lang

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


formatListItem :: (String -> Maybe B.Builder) -> (Int, Blocks) -> String
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


formatMarkdownInline :: Bool -> Inline -> String
formatMarkdownInline fixSpecialChars inline =
    case inline of
        Str text ->
            Text.unpack $ (if fixSpecialChars then Text.concatMap fix else id) text
        Space ->
            " "
        SoftBreak ->
            "\n"
        LineBreak ->
            "\n"
        Emph inlines ->
            "_" ++ (fold $ fmap (formatMarkdownInline True) $ inlines) ++ "_" -- TODO: escaping
        Strong inlines ->
            "**" ++ (fold $ fmap (formatMarkdownInline True) $ inlines) ++ "**" -- TODO: escaping
        Code text ->
            case longestSpanOf '`' text of
                NoSpan -> "`" ++ Text.unpack text ++ "`"
                Span n ->
                    let
                        delimiter = replicate (n + 1) '`'
                    in
                    delimiter ++ " " ++ Text.unpack text ++ " " ++ delimiter

        Link inlines (Url url) title ->
            let
                text = fold $ fmap (formatMarkdownInline fixSpecialChars) $ inlines
                textRaw = fold $ fmap (formatMarkdownInline False) $ inlines

                title' = Text.unpack title
                url' = Text.unpack url

                isValidAutolink =
                    Regex.match absoluteUrlRegex
            in
                if textRaw == url' && title' == "" && isValidAutolink url'
                    then
                        if fixSpecialChars
                            then "<" ++ url' ++ ">"
                            else url'
                    else
                        "[" ++ text
                            ++ "](" ++ Text.unpack url
                            ++ (if title' == "" then "" else " \"" ++ title' ++ "\"")
                            ++ ")"

        Link inlines (Ref ref) _ ->
            let
                text = fold $ fmap (formatMarkdownInline fixSpecialChars) $ inlines

                ref' = Text.unpack ref
            in
                if text == ref' || ref' == ""
                    then "[" ++ text ++ "]"
                    else "[" ++ text ++ "][" ++ ref' ++ "]"

        Image inlines url title ->
            "![" ++ (fold $ fmap (formatMarkdownInline fixSpecialChars) $ inlines)
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


{-| As defined at https://spec.commonmark.org/0.29/#autolinks -}
absoluteUrlRegex :: Regex.Regex Char
absoluteUrlRegex =
    schemeInitial * Regex.plus schemeSubsequent * Regex.once ':' * Regex.star urlSafe
    where
        schemeInitial = Regex.satisfy isSchemeInitial
        schemeSubsequent = Regex.satisfy isSchemeSubsequent
        urlSafe = Regex.satisfy isUrlSafe

        isSchemeInitial c =
            isAsciiUpper c || isAsciiLower c

        isSchemeSubsequent c =
            isAsciiUpper c || isAsciiLower c || isAsciiDigit c || c == '+' || c == '.' || c == '-'

        isUrlSafe c =
            not (Char.isSpace c && c == '<' && c == '>')

        isAsciiUpper c =
            c >= 'A' && c <= 'Z'

        isAsciiLower c =
            c >= 'a' && c <= 'z'

        isAsciiDigit c =
            c >= '0' && c <= '9'
