module ElmFormat.Render.Markdown where

import Cheapskate.Types
import Data.Foldable (fold)
import qualified Control.Applicative
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Text (Text)


-- TODO: remove this after upgrading to containers-0.5.8
intersperse :: a -> Seq.Seq a -> Seq.Seq a
intersperse y xs = case Seq.viewl xs of
  Seq.EmptyL -> Seq.empty
  p Seq.:< ps -> p Seq.<| (ps Control.Applicative.<**> (const y Seq.<| Seq.singleton id))


formatMarkdown :: (String -> Maybe String) -> Blocks -> String
formatMarkdown formatCode blocks =
    let
        needsInitialBlanks =
            case Seq.viewl blocks of
                (Para _ Seq.:< _) -> False
                Seq.EmptyL -> False
                _ -> True
    in
        (if needsInitialBlanks then "\n\n" else "") ++ (fold $ intersperse "\n" $ fmap (formatMardownBlock formatCode) $ blocks)


formatMardownBlock :: (String -> Maybe String) -> Block -> String
formatMardownBlock formatCode block =
    case block of
        Para inlines ->
            (fold $ fmap formatMarkdownInline $ inlines) ++ "\n"
        Header level inlines ->
            replicate level '#' ++ " " ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "\n"
        Blockquote blocks ->
            "TODO:Markdown.Blockquote"
        List tight (Bullet b) items ->
            fold $ (if tight then id else List.intersperse "\n") $
                fmap (fold . fmap (unlines . prefix ("  * ") "    " . lines . formatMardownBlock formatCode)) items
        List tight (Numbered _ i) items ->
            fold $ (if tight then id else List.intersperse "\n") $
                fmap (formatListItem formatCode) $ zip [1..] items

        CodeBlock (CodeAttr lang _) code ->
            unlines $ fmap ((++) "    ") $ lines $ fromMaybe (Text.unpack code) $ formatCode $ Text.unpack code
        HtmlBlock text ->
            "TODO:Markdown.HtmlBlock"
        HRule ->
            "TODO:Markdown.HRule"
        ReferencesBlock refs ->
            fold $ fmap formatRef refs


formatListItem :: (String -> Maybe String) -> (Int, Blocks) -> String
formatListItem formatCode (i, item)=
    let
        pref =
            if i < 10
                then show i ++ ".  "
                else show i ++ ". "

        addPrefix =
            unlines . prefix pref "    " . lines
    in
        fold $ fmap (addPrefix . formatMardownBlock formatCode) item


formatRef :: (Text, Text, Text) -> String
formatRef (label, url, title) =
    "[" ++ Text.unpack label ++ "]: " ++ Text.unpack url ++ "\n"


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
            "TODO:Markdown.LineBreak"
        Emph inlines ->
            "*" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "*" -- TODO: escaping
        Strong inlines ->
            "**" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "**" -- TODO: escaping
        Code text ->
            "`" ++ Text.unpack text ++ "`" -- TODO: escape backticks
        Link inlines (Url url) text ->
            "[" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "](" ++ Text.unpack url ++ ")"
        Link inlines (Ref ref) text ->
            "[" ++ (fold $ fmap formatMarkdownInline $ inlines) ++ "][" ++ Text.unpack ref ++ "]"
        Image inlines url text ->
            "TODO:Markdown.Image"
        Entity text ->
            Text.unpack text
        RawHtml text ->
            "TODO:Markdown.RawHtml"
