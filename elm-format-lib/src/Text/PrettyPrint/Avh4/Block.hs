module Text.PrettyPrint.Avh4.Block
  ( Line, identifier, keyword, punc, literal, space
  , Block(SingleLine, MustBreak), blankLine, line, mustBreak, stack', andThen
  , isLine
  , indent, prefix, addSuffix
  , render
  ,lineLength,comment,stack,joinMustBreak,prefixOrIndent, rowOrStack, rowOrStack', rowOrIndent, rowOrIndent') where

import Data.Fix

import qualified Data.Text as T
import Text.PrettyPrint.Avh4.Indent (Indent)
import qualified Text.PrettyPrint.Avh4.Indent as Indent
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NonEmpty


{-
A line is ALWAYS just one line.

Space is self-explanatory,
  Tab aligns to the nearest multiple of 4 spaces,
  Text brings any string into the data structure,
  Row joins more of these elements onto one line.
-}
data LineF a
    = Text T.Text
    | Row a a
    | Space

type Line = Fix LineF

instance Semigroup Line where
    a <> b = Fix $ Row a b


identifier :: T.Text -> Line
identifier =
    Fix . Text


keyword :: T.Text -> Line
keyword =
    Fix . Text


punc :: T.Text -> Line
punc =
    Fix . Text


literal :: T.Text -> Line
literal =
    Fix . Text


comment :: T.Text -> Line
comment =
    Fix . Text


space :: Line
space =
    Fix Space


data Indented a =
    Indented Indent a
    deriving (Functor)


{-
Block contains Lines (at least one - can't be empty).
Block either:
  - can appear in the middle of a line
      (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
      (Stack someLine moreLines OR MustBreak someLine).

MustBreak is only used for `--` comments.

Stack contains two or more lines.

Sometimes (see `prefix`) the first line of Stack
  gets different treatment than the other lines.
-}
data Block
    = SingleLine (Indented Line)
    | Stack (Indented Line) (Indented Line) [Indented Line]
    | MustBreak (Indented Line)


blankLine :: Block
blankLine =
    line $ literal ""


line :: Line -> Block
line =
    SingleLine . mkIndentedLine


mustBreak :: Line -> Block
mustBreak =
    MustBreak . mkIndentedLine


mkIndentedLine :: Line -> Indented Line
mkIndentedLine (Fix Space) = Indented (Indent.spaces 1) (literal "")
mkIndentedLine (Fix (Row (Fix Space) next)) =
    let (Indented i rest') = mkIndentedLine next
    in
    Indented (Indent.spaces 1 <> i) rest'
mkIndentedLine other = Indented mempty other


stack' :: Block -> Block -> Block
stack' b1 b2 =
    let
        (line1first, line1rest) = destructure b1
        (line2first, line2rest) = destructure b2
    in case line1rest ++ line2first:line2rest of
        [] ->
            error "the list will contain at least line2first"
        first : rest ->
            Stack line1first first rest


andThen :: [Block] -> Block -> Block
andThen rest first =
    foldl stack' first rest


stack :: NonEmpty Block -> Block
stack = foldr1 stack'


joinMustBreak :: Block -> Block -> Block
joinMustBreak inner eol =
    case (inner, eol) of
        (SingleLine (Indented i1 inner'), SingleLine (Indented _ eol')) ->
            SingleLine $ Indented i1 $
            inner' <> space <> eol'

        (SingleLine (Indented i1 inner'), MustBreak (Indented _ eol')) ->
            MustBreak $ Indented i1 $
            inner' <> space <> eol'

        _ ->
            stack' inner eol


{-# INLINE prefixOrIndent #-}
prefixOrIndent :: Maybe Line -> Line -> Block -> Block
prefixOrIndent joiner a b =
    let
        join a b =
            case joiner of
                Nothing -> a <> b
                Just j -> a <> j <> b
    in
    case b of
        SingleLine (Indented _ b') ->
            line $ join a b'

        MustBreak (Indented _ b') ->
            mustBreak $ join a b'

        _ ->
            stack' (line a) (indent b)


mapLines :: (Indented Line -> Indented Line) -> Block -> Block
mapLines fn =
    mapFirstLine fn fn


mapFirstLine :: (Indented Line -> Indented Line) -> (Indented Line -> Indented Line) -> Block -> Block
mapFirstLine firstFn restFn b =
    case b of
        SingleLine l1 ->
            SingleLine (firstFn l1)
        Stack l1 l2 ls ->
            Stack (firstFn l1) (restFn l2) (map restFn ls)
        MustBreak l1 ->
            MustBreak (firstFn l1)


mapLastLine :: (Indented Line -> Indented Line) -> Block -> Block
mapLastLine lastFn = \case
    SingleLine l1 ->
        SingleLine (lastFn l1)
    Stack l1 l2 [] ->
        Stack l1 (lastFn l2) []
    Stack l1 l2 ls ->
        Stack l1 l2 (init ls ++ [lastFn $ last ls])
    MustBreak l1 ->
        MustBreak (lastFn l1)


indent :: Block -> Block
indent =
    mapLines (\(Indented i l) -> Indented (Indent.tab <> i) l)


{-# INLINE rowOrStack #-}
rowOrStack :: Maybe Line -> NonEmpty Block -> Block
rowOrStack = rowOrStack' False

{-# INLINE rowOrStack' #-}
rowOrStack' :: Bool -> Maybe Line -> NonEmpty Block -> Block
rowOrStack' _ _ (single :| []) = single
rowOrStack' forceMultiline (Just joiner) blocks =
    case allSingles blocks of
        Right lines | not forceMultiline ->
            line $ sconcat $ NonEmpty.intersperse joiner lines
        _ ->
            stack blocks
rowOrStack' forceMultiline Nothing blocks =
    case allSingles blocks of
        Right lines | not forceMultiline ->
            line $ sconcat lines
        _ ->
            stack blocks


{-# INLINE rowOrIndent #-}
rowOrIndent :: Maybe Line -> NonEmpty Block -> Block
rowOrIndent = rowOrIndent' False


{-# INLINE rowOrIndent' #-}
rowOrIndent' :: Bool -> Maybe Line -> NonEmpty Block -> Block
rowOrIndent' _ _ (single :| []) = single
rowOrIndent' forceMultiline (Just joiner) blocks@(b1 :| rest) =
    case allSingles blocks of
        Right lines | not forceMultiline ->
            line $ sconcat $ NonEmpty.intersperse joiner lines
        _ ->
            stack (b1 :| (indent <$> rest))
rowOrIndent' forceMultiline Nothing blocks@(b1 :| rest) =
    case allSingles blocks of
        Right lines | not forceMultiline ->
            line $ sconcat lines
        _ ->
            stack (b1 :| (indent <$> rest))


{-# DEPRECATED isLine "Rewrite to avoid inspecting the child blocks" #-}
isLine :: Block -> Either Block Line
isLine b =
    case b of
        SingleLine (Indented _ l) ->
            Right l
        _ ->
            Left b


destructure :: Block -> (Indented Line, [Indented Line])
destructure b =
    case b of
        SingleLine l1 ->
            (l1, [])
        Stack l1 l2 rest ->
            (l1, l2 : rest)
        MustBreak l1 ->
            (l1, [])


allSingles :: Traversable t => t Block -> Either (t Block) (t Line)
allSingles blocks =
    case mapM isLine blocks of
        Right lines' ->
            Right lines'
        _ ->
            Left blocks


{-
Add the prefix to the first line,
pad the other lines with spaces of the same length

NOTE: An exceptional case that we haven't really designed for is if the first line of the input Block is indented.

EXAMPLE:
abcde
xyz
----->
myPrefix abcde
         xyz
-}
prefix :: Line -> Block -> Block
prefix pref =
    let
        prefixLength = fromIntegral $ T.length $ renderLine pref
        padLineWithSpaces (Indented i l) = Indented (Indent.spaces prefixLength <> i) l
        addPrefixToLine l = pref <> l
    in
    mapFirstLine (fmap addPrefixToLine) padLineWithSpaces


addSuffix :: Line -> Block -> Block
addSuffix suffix =
    mapLastLine $ fmap (<> suffix)


renderIndentedLine :: Indented T.Text -> T.Text
renderIndentedLine (Indented i line') =
    T.replicate (Indent.width i) " " <> line'


renderLine :: Line -> T.Text
renderLine line' =
    case unFix line' of
        Text text ->
            text
        Space ->
            T.singleton ' '
        Row left right ->
            renderLine left <> renderLine right


render :: Block -> T.Text
render block' =
    case block' of
        SingleLine line' ->
            T.snoc (T.stripEnd $ renderIndentedLine $ renderLine <$> line') '\n'
        Stack l1 l2 rest ->
            T.unlines $ map (T.stripEnd . renderIndentedLine . fmap renderLine) (l1 : l2 : rest)
        MustBreak line' ->
            T.snoc (T.stripEnd $ renderIndentedLine $ renderLine <$> line') '\n'


lineLength :: Line -> Int
lineLength = T.length . renderLine
