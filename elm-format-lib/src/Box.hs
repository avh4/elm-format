{-# OPTIONS_GHC -Wall #-}
module Box
  ( Line, identifier, keyword, punc, literal, row, space
  , Box(SingleLine, MustBreak), blankLine, line, mustBreak, stack', stack1, andThen
  , isLine, allSingles
  , indent, prefix, addSuffix
  , render
  ,allSingles2,allSingles3,lineLength,isSingle,isMustBreak,comment,stack,joinMustBreak,prefixOrIndent) where

import Data.Fix
import Elm.Utils (List)

import qualified Data.Text as T
import Indent (Indent)
import qualified Indent
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty((:|)))


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


{-# DEPRECATED row "use `(<>)` instead" #-}
{-| join more Line elements into one
-}
row :: List Line -> Line
row [] = error $ "<elm-format: " ++ "UNEXPECTED ROW" ++ ": " ++ "no elements" ++ " -- please report this at https://github.com/avh4/elm-format/issues >"
row (first:rest) = sconcat (first:|rest)


space :: Line
space =
    Fix Space


data Indented a =
    Indented Indent a
    deriving (Functor)


{-
Box contains Lines (at least one - can't be empty).
Box either:
  - can appear in the middle of a line
      (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
      (Stack someLine moreLines OR MustBreak someLine).

MustBreak is only used for `--` comments.

Stack contains two or more lines.

Sometimes (see `prefix`) the first line of Stack
  gets different treatment than the other lines.
-}
data Box
    = SingleLine (Indented Line)
    | Stack (Indented Line) (Indented Line) [Indented Line]
    | MustBreak (Indented Line)


blankLine :: Box
blankLine =
    line $ literal ""


line :: Line -> Box
line =
    SingleLine . mkIndentedLine


mustBreak :: Line -> Box
mustBreak =
    MustBreak . mkIndentedLine


mkIndentedLine :: Line -> Indented Line
mkIndentedLine (Fix Space) = Indented (Indent.spaces 1) (literal "")
mkIndentedLine (Fix (Row (Fix Space) next)) =
    let (Indented i rest') = mkIndentedLine next
    in
    Indented (Indent.spaces 1 <> i) rest'
mkIndentedLine other = Indented mempty other


stack' :: Box -> Box -> Box
stack' b1 b2 =
    let
        (line1first, line1rest) = destructure b1
        (line2first, line2rest) = destructure b2
    in case line1rest ++ line2first:line2rest of
        [] ->
            error "the list will contain at least line2first"
        first : rest ->
            Stack line1first first rest


andThen :: [Box] -> Box -> Box
andThen rest first =
    foldl stack' first rest


stack :: Box -> [Box] -> Box
stack first rest = stack1 (first:rest)


{-# DEPRECATED stack1 "Prefer `stack` or `stack'`" #-}
stack1 :: [Box] -> Box
stack1 children =
    case children of
        [] ->
            error "stack1: empty structure"
        [first] ->
            first
        boxes ->
            foldr1 stack' boxes


joinMustBreak :: Box -> Box -> Box
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


prefixOrIndent :: Box -> Box -> Box
prefixOrIndent a b =
    case ( a, b ) of
        (SingleLine (Indented i1 a'), SingleLine (Indented _ b')) ->
            SingleLine $ Indented i1 $
            a' <> space <> b'

        (SingleLine (Indented i1 a'), MustBreak (Indented _ b')) ->
            MustBreak $ Indented i1 $
            a' <> space <> b'

        _ ->
            stack' a (indent b)


mapLines :: (Indented Line -> Indented Line) -> Box -> Box
mapLines fn =
    mapFirstLine fn fn


mapFirstLine :: (Indented Line -> Indented Line) -> (Indented Line -> Indented Line) -> Box -> Box
mapFirstLine firstFn restFn b =
    case b of
        SingleLine l1 ->
            SingleLine (firstFn l1)
        Stack l1 l2 ls ->
            Stack (firstFn l1) (restFn l2) (map restFn ls)
        MustBreak l1 ->
            MustBreak (firstFn l1)


mapLastLine :: (Indented Line -> Indented Line) -> Box -> Box
mapLastLine lastFn = \case
    SingleLine l1 ->
        SingleLine (lastFn l1)
    Stack l1 l2 [] ->
        Stack l1 (lastFn l2) []
    Stack l1 l2 ls ->
        Stack l1 l2 (init ls ++ [lastFn $ last ls])
    MustBreak l1 ->
        MustBreak (lastFn l1)


indent :: Box -> Box
indent =
    mapLines (\(Indented i l) -> Indented (Indent.tab <> i) l)


isLine :: Box -> Either Box Line
isLine b =
    case b of
        SingleLine (Indented _ l) ->
            Right l
        _ ->
            Left b


isSingle :: Box -> Maybe Line
isSingle b =
    case b of
        SingleLine (Indented _ l) ->
            Just l
        _ ->
            Nothing


isMustBreak :: Box -> Maybe Line
isMustBreak b =
    case b of
        MustBreak (Indented _ l) ->
            Just l
        _ ->
            Nothing


destructure :: Box -> (Indented Line, [Indented Line])
destructure b =
    case b of
        SingleLine l1 ->
            (l1, [])
        Stack l1 l2 rest ->
            (l1, l2 : rest)
        MustBreak l1 ->
            (l1, [])


allSingles :: Traversable t => t Box -> Either (t Box) (t Line)
allSingles boxes =
    case mapM isLine boxes of
        Right lines' ->
            Right lines'
        _ ->
            Left boxes


allSingles2 :: Box -> Box -> Either (Box, Box) (Line, Line)
allSingles2 b1 b2 =
    case allSingles [b1, b2] of
        Right [l1, l2] -> Right (l1, l2)
        _ -> Left (b1, b2)


allSingles3 :: Box -> Box -> Box -> Either (Box, Box, Box) (Line, Line, Line)
allSingles3 b1 b2 b3 =
    case allSingles [b1, b2, b3] of
        Right [l1, l2, l3] -> Right (l1, l2, l3)
        _ -> Left (b1, b2, b3)


{-
Add the prefix to the first line,
pad the other lines with spaces of the same length

NOTE: An exceptional case that we haven't really designed for is if the first line of the input Box is indented.

EXAMPLE:
abcde
xyz
----->
myPrefix abcde
         xyz
-}
prefix :: Line -> Box -> Box
prefix pref =
    let
        prefixLength = fromIntegral $ T.length $ renderLine pref
        padLineWithSpaces (Indented i l) = Indented (Indent.spaces prefixLength <> i) l
        addPrefixToLine l = pref <> l
    in
    mapFirstLine (fmap addPrefixToLine) padLineWithSpaces


addSuffix :: Line -> Box -> Box
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


render :: Box -> T.Text
render box' =
    case box' of
        SingleLine line' ->
            T.snoc (T.stripEnd $ renderIndentedLine $ renderLine <$> line') '\n'
        Stack l1 l2 rest ->
            T.unlines $ map (T.stripEnd . renderIndentedLine . fmap renderLine) (l1 : l2 : rest)
        MustBreak line' ->
            T.snoc (T.stripEnd $ renderIndentedLine $ renderLine <$> line') '\n'


lineLength :: Line -> Int
lineLength = T.length . renderLine
