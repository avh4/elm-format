module Text.PrettyPrint.Avh4.Block
  ( Line
  , identifier, identifierByteString, identifierText
  , keyword, keywordByteString, keywordText
  , punc, puncByteString, puncText
  , literal, literalByteString, literalText
  , commentByteString, commentText
  , space

  , Block(SingleLine, MustBreak), blankLine, line, mustBreak
  , stack, stack', andThen
  , indent, prefix, addSuffix, joinMustBreak
  , prefixOrIndent, rowOrStack, rowOrStack', rowOrIndent, rowOrIndent'
  , render

  -- DEPRECATED
  , isLine, lineLength
  ) where

import Data.Text (Text)
import Text.PrettyPrint.Avh4.Indent (Indent)
import qualified Text.PrettyPrint.Avh4.Indent as Indent
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString


{-| A `Line` is ALWAYS just one single line of text,
and can always be combined horizontally with other `Line`s.

- `Space` is a single horizontal space,
- `Blank` is a line with no content.
- `Text` brings any text into the data structure. (Uses `ByteString.Builder` for the possibility of optimal performance)
- `Row` joins multiple elements onto one line.

-}
data Line
    = Text B.Builder
    | Row Line Line
    | Space
    | Blank

instance Semigroup Line where
    a <> b = Row a b


{-# INLINE mkTextText #-}
mkTextText :: Text -> Line
mkTextText = mkTextByteString . Data.Text.Encoding.encodeUtf8


{-# INLINE mkTextByteString #-}
mkTextByteString :: ByteString -> Line
mkTextByteString = Text . B.byteString


identifier :: B.Builder -> Line
identifier = Text

{-| For better performance, if you can avoid allocating the `ByteString`, use `identifier` instead. -}
identifierByteString :: ByteString -> Line
identifierByteString = mkTextByteString

{-| For better performance, if you can avoid allocating the `Text`, use `identifierByteString` or `identifier` instead. -}
identifierText :: Text -> Line
identifierText = mkTextText


keyword :: B.Builder -> Line
keyword = Text

{-| For better performance, if you can avoid allocating the `ByteString`, use `keyword` instead. -}
keywordByteString :: ByteString -> Line
keywordByteString = mkTextByteString

{-| For better performance, if you can avoid allocating the `Text`, use `keywordByteString` or `keyword` instead. -}
keywordText :: Text -> Line
keywordText = mkTextText


punc :: B.Builder -> Line
punc = Text

{-| For better performance, if you can avoid allocating the `ByteString`, use `punc` instead. -}
puncByteString :: ByteString -> Line
puncByteString = mkTextByteString

{-| For better performance, if you can avoid allocating the `Text`, use `puncByteString` or `punc` instead. -}
puncText :: Text -> Line
puncText = mkTextText


literal :: B.Builder -> Line
literal = Text

{-| For better performance, if you can avoid allocating the `ByteString`, use `literal` instead. -}
literalByteString :: ByteString -> Line
literalByteString = mkTextByteString

{-| For better performance, if you can avoid allocating the `Text`, use `literalByteString` or `literal` instead. -}
literalText :: Text -> Line
literalText = mkTextText


commentByteString :: ByteString -> Line
commentByteString bs =
    if ByteString.null bs
        then Blank
        else mkTextByteString bs

{-| For better performance, if you can avoid allocating the `Text`, use `commentByteString` instead. -}
commentText :: Text -> Line
commentText text =
    if Text.null text
        then Blank
        else mkTextText text


space :: Line
space =
    Space


data Indented a =
    Indented Indent a
    deriving (Functor)


{-| `Block` contains Lines (at least one; it can't be empty).

Block either:
  - can appear in the middle of a line
      (Stack someLine [], thus can be joined without problems), or
  - has to appear on its own
      (Stack someLine moreLines OR MustBreak someLine).

- `SingleLine` is a single line, and the indentation level for the line.
- `MustBreak` is a single line (and its indentation level)) that cannot have anything joined to its right side.
  Notably, it is used for `--` comments.
- `Stack` contains two or more lines, and the indentation level for each.

Sometimes (see `prefix`) the first line of Stack
  gets different treatment than the other lines.
-}
data Block
    = SingleLine (Indented Line)
    | Stack (Indented Line) (Indented Line) [Indented Line]
    | MustBreak (Indented Line)


blankLine :: Block
blankLine =
    line Blank


line :: Line -> Block
line =
    SingleLine . mkIndentedLine


mustBreak :: Line -> Block
mustBreak =
    MustBreak . mkIndentedLine


mkIndentedLine :: Line -> Indented Line
mkIndentedLine Space = Indented (Indent.spaces 1) Blank
mkIndentedLine (Row Space next) =
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
prefix :: Word -> Line -> Block -> Block
prefix prefixLength pref =
    let
        padLineWithSpaces (Indented i l) = Indented (Indent.spaces prefixLength <> i) l

        addPrefixToLine Blank = stripEnd pref
        addPrefixToLine l = pref <> l
    in
    mapFirstLine (fmap addPrefixToLine) padLineWithSpaces


stripEnd :: Line -> Line
stripEnd = \case
    Space -> Blank
    Row r1 r2 ->
        case (stripEnd r1, stripEnd r2) of
            (r1', Blank) -> r1'
            (Blank, r2') -> r2'
            (r1', r2') -> Row r1' r2'
    Text t -> Text t
    Blank -> Blank


addSuffix :: Line -> Block -> Block
addSuffix suffix =
    mapLastLine $ fmap (<> suffix)


renderIndentedLine :: Indented Line -> B.Builder
renderIndentedLine (Indented i line') =
    renderLine i line' <> B.char7 '\n'


spaces :: Int -> B.Builder
spaces i =
    B.byteString (ByteString.replicate i 0x20 {- space -})


renderLine :: Indent -> Line -> B.Builder
renderLine i = \case
    Text text ->
        spaces (Indent.width i) <> text
    Space ->
        spaces (1 + Indent.width i)
    Row left right ->
        renderLine i left <> renderLine mempty right
    Blank ->
        mempty


render :: Block -> B.Builder
render = \case
    SingleLine line' ->
        renderIndentedLine line'
    Stack l1 l2 rest ->
        foldMap renderIndentedLine (l1 : l2 : rest)
    MustBreak line' ->
        renderIndentedLine line'


{-# DEPRECATED lineLength "Instead, determine the length from your inputs instead of calculating it after formatting. This is deprecated because it prevents ByteString.Builder performance optimization best practices." #-}
lineLength :: Line -> Int
lineLength = fromIntegral . Data.Text.Lazy.length . Data.Text.Lazy.Encoding.decodeUtf8 . B.toLazyByteString . renderLine mempty
