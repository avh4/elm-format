{-# LANGUAGE OverloadedLists #-}

module ElmFormat.Render.ElmStructure
  ( spaceSepOrStack, forceableSpaceSepOrStack, forceableSpaceSepOrStack1
  , forceableRowOrStack
  , spaceSepOrIndented, forceableSpaceSepOrIndented, spaceSepOrPrefix, prefixOrIndented
  , equalsPair, definition
  , application, group, group', extensionGroup, render,keyword, Elm,groupOfOne,parens,module',identifier,docComment,import',stackWithVariableSpacing,stack1,commentBlock,mustBreakComment,literal,case',caseBranch,unionListing,stackIndent,letIn,ifElse,spaceSepMustBreak,lambda,sectionedGroup,suffix,range,unary, line)
  where


import Elm.Utils ((|>))
import Text.PrettyPrint.Avh4.Block
    ( Block(..),
      space,
      indent,
      isLine,
      lineLength,
      blankLine )
import AST.V0_16 (FunctionApplicationMultiline(..), Multiline(..))

import Data.Fix (Fix (Fix))

import qualified Text.PrettyPrint.Avh4.Block as Block
import qualified Data.List as List
import Data.Maybe (maybeToList, catMaybes)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty


data ElmF a
    = Keyword Text
    | Literal Text
    | Identifier Text
    | DocComment Text Text [Text]
    | CommentBlock Text [Text] Text
    | MustBreakComment Text
    | BoxLine Block.Line
    | Suffix a Block.Line
    | WrapStack a a [a]
    | WrapStackNoSpaces a a [a]
    | WrapIndent a a [a]
    | JoinMustBreak a a
    | Stack a (Int, a) [(Int, a)]
    | StackIndent a a [a]
    | PrefixOrIndent Text a
    | EqualsPair Bool a Text a
    | FunctionApplication FunctionApplicationMultiline a (NonEmpty a)
    | Case Text Text Bool a [a]
    | CaseClause Bool a Text a
    | LetIn Text a Text a
    | IfElse Text a Text a [(Maybe a, a, a)] Text a
    | Lambda Text Text Bool a (Maybe a) a
    | SectionedGroup Bool Text Text Text Bool (NonEmpty a) [(a, NonEmpty a)] (Maybe a)
    | GroupOfOne Text a Text
    | ExtensionGroup Text Text Text Text Bool a (NonEmpty a) [(a, NonEmpty a)] (Maybe a)
    | Range Text Text Text a a
    | OperatorPrefix Bool Block.Line a
    | Module [a] (Maybe a, Maybe a, (Maybe a, [a])) Int (Maybe a)
    | Import a (Maybe a) (Maybe a)
    | UnionListing a Bool (Maybe a)
    deriving (Functor)

type Elm = Fix ElmF


render :: ElmF Block -> Block
render = \case
    Keyword k ->
        Block.line $ Block.keyword k

    Literal l ->
        Block.line $ Block.literal l

    Identifier name ->
        Block.line $ Block.identifier name

    CommentBlock left inner right ->
        case inner of
            [] ->
                Block.line $ Block.comment (left <> " " <> right)

            [single] ->
                Block.line $ Block.comment (left <> " " <> single <> " " <> right)

            (first:rest) ->
                Block.stack'
                    (Block.prefix
                        (Block.comment (left <> " "))
                        (Block.stack $ Block.line . Block.comment <$>
                            first :| rest
                        )
                    )
                    (Block.line $ Block.comment right)

    MustBreakComment line ->
        Block.mustBreak $ Block.comment line

    BoxLine line ->
        Block.line line

    Suffix a suf ->
        Block.addSuffix suf a

    DocComment left right lines' ->
        case lines' of
            [] ->
                Block.line $ Block.punc left <> space <> Block.punc right

            [first] ->
                Block.stack'
                    (Block.line $ Block.punc left <> space <> Block.literal first)
                    (Block.line $ Block.punc right)

            (first:rest) ->
                Block.line (Block.punc left <> space <> Block.literal first)
                    |> Block.andThen (map (Block.line . Block.literal) rest)
                    |> Block.andThen [ Block.line $ Block.punc right ]

    WrapStack a b rest ->
        Block.rowOrStack
            (Just space)
            (a :| b : rest)

    WrapStackNoSpaces a b rest ->
        Block.rowOrStack
            Nothing
            (a :| b : rest)

    WrapIndent a b rest ->
        Block.rowOrIndent
            (Just space)
            (a :| b : rest)

    JoinMustBreak inner eol ->
        Block.joinMustBreak inner eol

    Stack a b rest ->
        Block.stack $ a :| mconcat (pair <$> b:rest)
        where
            pair (n, x) =
                List.replicate n Block.blankLine ++ [x]

    StackIndent a b rest ->
        Block.stack $ a :| (indent <$> (b:rest))

    PrefixOrIndent prefix b ->
        Block.prefixOrIndent (Just space) (Block.punc prefix) b

    EqualsPair forceMultiline left symbol right ->
        Block.rowOrIndent' forceMultiline
            (Just space)
            [ Block.rowOrIndent (Just space)
                [ left
                , Block.line $ Block.punc symbol
                ]
            , right
            ]

    FunctionApplication forceMultiline first (arg0:|rest) ->
        let
            (forceFirst, forceRest) =
                case forceMultiline of
                    FAJoinFirst JoinAll -> (False, False)
                    FAJoinFirst SplitAll -> (False, True)
                    FASplitFirst -> (True, True)
        in
        Block.rowOrIndent' forceRest (Just space) $
            Block.rowOrIndent' forceFirst (Just space)
                [ first
                , arg0
                ]
            :| rest

    Case caseWord ofWord forceMultilineSubject subject clauses ->
        let
            opening =
                Block.rowOrStack
                    (Just space)
                    [ Block.rowOrIndent'
                        forceMultilineSubject
                        (Just space)
                        [ Block.line $ Block.keyword caseWord
                        , subject
                        ]
                    , Block.line $ Block.keyword ofWord
                    ]
        in
        Block.stack $
            opening :|
            (indent <$> List.intersperse blankLine clauses)

    CaseClause False pattern arrow body ->
        Block.stack'
            (Block.addSuffix (space <> Block.keyword arrow) pattern)
            (Block.indent body)

    CaseClause True pattern arrow body ->
        Block.stack
            [ pattern
            , Block.line $ Block.keyword arrow
            , Block.indent body
            ]

    LetIn letWord defs inWord body ->
        Block.stack
            [ Block.line $ Block.keyword letWord
            , Block.indent defs
            , Block.line $ Block.keyword inWord
            , body
            ]

    IfElse ifWord condition thenWord ifBody elseIfs elseWord elseBody ->
        let
            opening key cond =
                render $ WrapStack
                    (render $ WrapIndent key cond [])
                    (Block.line $ Block.keyword thenWord)
                    []

            formatElseIf (ifComments, cond, body) =
                let
                    if' =
                        case ifComments of
                            Nothing-> Block.line $ Block.keyword ifWord
                            Just c ->
                                render $ WrapStack c (Block.line $ Block.keyword ifWord) []

                    key =
                        render $ WrapStack
                            (Block.line $ Block.keyword elseWord)
                            if'
                            []
                in
                Block.stack
                    [ blankLine
                    , opening key cond
                    , indent body
                    ]
        in
        Block.stack'
            (opening (Block.line $ Block.keyword ifWord) condition)
            (indent ifBody)
            |> Block.andThen (formatElseIf <$> elseIfs)
            |> Block.andThen
                [ blankLine
                , Block.line $ Block.keyword elseWord
                , indent elseBody
                ]

    Lambda start arrow forceMultiline args bodyComments body ->
        Block.rowOrIndent' forceMultiline (Just space)
            [ Block.rowOrStack
                (Just space)
                [ Block.prefix (Block.punc start) args
                , Block.line $ Block.punc arrow
                ]
            , Block.stack $ NonEmpty.fromList $ catMaybes
                [ bodyComments
                , Just body
                ]
            ]

    SectionedGroup innerSpaces left sep right forceMultiline section1 moreSections extraFooter ->
        let
            forceMultiline' =
                forceMultiline || not (null moreSections)

            forceWrap wrap =
                if forceMultiline' then stack0 else wrap

            final =
                case extraFooter of
                    Nothing -> Block.line $ Block.punc right
                    Just footer ->
                        Block.stack
                            [ blankLine
                            , footer
                            , Block.line $ Block.punc right
                            ]

            attempt innerSpaces' =
                render $ forceWrap (if innerSpaces' then WrapStack else WrapStackNoSpaces)
                    (renderSections innerSpaces' forceMultiline' left sep section1 moreSections)
                    final
                    []

            withInnerSpaces =
                attempt True
        in
        case (innerSpaces, isLine withInnerSpaces) of
            (False, Right _) -> attempt False
            _ -> withInnerSpaces

    GroupOfOne left inner right ->
        Block.rowOrStack Nothing
            [ Block.prefix (Block.punc left) inner
            , Block.line (Block.punc right)
            ]

    ExtensionGroup left delim sep right forceMultiline base section1 moreSections extraFooter ->
        let
            extraFooter' =
                case extraFooter of
                    Nothing -> []
                    Just f -> [ blankLine, f ]
        in
        render $ (if forceMultiline then stack0 else WrapStack)
            (render $ (if forceMultiline then StackIndent else WrapIndent)
                (render $ OperatorPrefix True (Block.punc left) base)
                (renderSections True forceMultiline delim sep section1 moreSections)
                extraFooter'
            )
            (Block.line $ Block.punc right)
            []

    Range left dots right a b ->
        Block.rowOrStack Nothing
            [ Block.prefixOrIndent Nothing (Block.punc left) a
            , Block.prefixOrIndent Nothing (Block.punc dots) b
            , Block.line $ Block.punc right
            ]

    OperatorPrefix False op rest ->
        if lineLength op < 4
            then Block.prefix op rest
            else Block.rowOrIndent Nothing [Block.line op, rest]

    OperatorPrefix True op rest ->
        if lineLength op < 4
            then Block.prefix (op <> space) rest
            else Block.rowOrIndent (Just space) [Block.line op, rest]

    Module initialComments (maybeHeader, docs, (importComments, imports)) spaceBeforeBody body ->
        let
            initialComments' =
                case initialComments of
                    [] -> []
                    some -> some <> [ blankLine, blankLine ]

            imports' =
                [ maybeToList importComments
                , imports
                ]
                    |> List.filter (not . List.null)
                    |> List.intersperse [blankLine]
                    |> concat
        in
        Block.stack $ NonEmpty.fromList $ concat @[]
            [ initialComments'
            , List.intercalate [ blankLine ] $ concat @[]
                [ maybeToList $ return <$> maybeHeader
                , maybeToList $ return <$> docs
                , [imports' | not (null imports')]
                ]
            , List.replicate spaceBeforeBody blankLine
            , maybeToList body
            ]

    Import name as exposing ->
        Block.rowOrIndent (Just space) $
            Block.rowOrIndent
                (Just space)
                (Block.line (Block.keyword "import")
                    :| catMaybes
                        [ Just name
                        , as
                        ]
                )
            :| catMaybes
                [ exposing
                ]

    UnionListing name _ Nothing ->
        name

    UnionListing name False (Just listing) ->
        Block.rowOrIndent Nothing [name, listing]

    UnionListing name True (Just listing) ->
        Block.rowOrIndent (Just space) [name, listing]


stack0 :: a -> a -> [a] -> ElmF a
stack0 a b rest =
    Stack a (0, b) ((,) 0 <$> rest)


renderSections :: Bool -> Bool -> Text -> Text -> NonEmpty Block -> [(Block, NonEmpty Block)] -> Block
renderSections innerSpaces forceMultiline left sep (first0':|firsts') moreSections' =
    let
        renderItem innerSpaces' punc item =
            render $ OperatorPrefix
                (innerSpaces' || punc == sep)
                (Block.punc punc)
                item

        renderLabeledSection innerSpaces' (label, items) =
            Block.stack
                [ blankLine
                , label
                , renderSections forceMultiline innerSpaces' sep sep items []
                ]
    in
    Block.rowOrStack' forceMultiline Nothing $
        renderItem innerSpaces left first0' :|
        (renderItem innerSpaces sep <$> firsts')
        ++ (renderLabeledSection innerSpaces <$> moreSections')


keyword :: Text -> Elm
keyword = Fix . Keyword


literal :: Text -> Elm
literal = Fix . Literal


identifier :: Text -> Elm
identifier = Fix . Identifier


parens :: Elm -> Elm
parens = groupOfOne "(" ")"


groupOfOne :: Text -> Text -> Elm -> Elm
groupOfOne left right inner =
    Fix $ GroupOfOne left inner right


docComment :: Text -> Text -> [Text] -> Elm
docComment left right lines' =
    Fix $ DocComment left right lines'


commentBlock :: Text -> Text -> [Text] -> Elm
commentBlock left right inner =
    Fix $ CommentBlock left inner right


mustBreakComment :: Text -> Elm
mustBreakComment = Fix . MustBreakComment


line :: Block.Line -> Elm
line = Fix . BoxLine


suffix :: Block.Line -> Elm -> Elm
suffix suf a =
    Fix $ Suffix a suf


module' :: [Elm] -> (Maybe Elm, Maybe Elm, (Maybe Elm, [Elm])) -> Int -> Maybe Elm -> Elm
module' initialComments header spaceBeforeBody body =
    Fix $ Module initialComments header spaceBeforeBody body


import' :: Elm -> Maybe Elm -> Maybe Elm -> Elm
import' name as exposing =
    Fix $ Import name as exposing


unionListing :: Elm -> Bool -> Maybe Elm -> Elm
unionListing name nameHasComments listing =
    Fix $ UnionListing name nameHasComments listing


{-| Same as `forceableSpaceSepOrStack False`
-}
spaceSepOrStack :: Elm -> [Elm] -> Elm
spaceSepOrStack a [] = a
spaceSepOrStack a (b:rest) = Fix $ WrapStack a b rest


{-|
Formats as:

    first rest0 rest1

    first
    rest0
    rest1
-}
forceableSpaceSepOrStack :: Bool -> Elm -> [Elm] -> Elm
forceableSpaceSepOrStack _ a [] = a
forceableSpaceSepOrStack True a (b:rest) = Fix $ stack0 a b rest
forceableSpaceSepOrStack False a (b:rest) = Fix $ WrapStack a b rest


{-| Like `forceableSpaceSepOrStack`, but doesn't add spaces when
everything remains on one line.
-}
forceableRowOrStack :: Bool -> Elm -> [Elm] -> Elm
forceableRowOrStack _ a [] = a
forceableRowOrStack True a (b:rest) = Fix $ stack0 a b rest
forceableRowOrStack False a (b:rest) = Fix $ WrapStackNoSpaces a b rest


{-| Same as `forceableSpaceSepOrStack`
-}
forceableSpaceSepOrStack1 :: Bool -> NonEmpty Elm -> Elm
forceableSpaceSepOrStack1 forceMultiline (first:|rest) =
    forceableSpaceSepOrStack forceMultiline first rest


stack1 :: NonEmpty Elm -> Elm
stack1 = forceableSpaceSepOrStack1 True


stackWithVariableSpacing :: Elm -> [(Int, Elm)] -> Elm
stackWithVariableSpacing a [] = a
stackWithVariableSpacing a (b:rest) =
    Fix $ Stack a b rest


{-|
Formats as:

    first rest0 rest1 rest2

    first
      rest0
      rest1
      rest2
-}
spaceSepOrIndented :: Elm -> [Elm] -> Elm
spaceSepOrIndented a [] = a
spaceSepOrIndented a (b:rest) = Fix $ WrapIndent a b rest


forceableSpaceSepOrIndented :: Bool -> Elm -> [Elm] -> Elm
forceableSpaceSepOrIndented _ a [] = a
forceableSpaceSepOrIndented True a (b:rest) = Fix $ StackIndent a b rest
forceableSpaceSepOrIndented False a (b:rest) = Fix $ WrapIndent a b rest


stackIndent :: Elm -> [Elm] -> Elm
stackIndent = forceableSpaceSepOrIndented True


{-|
Formats as:

    op rest

    op rest1
       rest2

    opLong
        rest
-}
spaceSepOrPrefix :: Block.Line -> Elm -> Elm
spaceSepOrPrefix op rest =
    Fix $ OperatorPrefix True op rest


spaceSepMustBreak :: Elm -> Elm -> Elm
spaceSepMustBreak inner eol =
    Fix $ JoinMustBreak inner eol


prefixOrIndented :: Text -> Elm -> Elm
prefixOrIndented pre body =
    Fix $ PrefixOrIndent pre body


{-|
Formats as:

    left = right

    left =
      right

    left
      =
      right
-}
equalsPair :: Text -> Bool -> Elm -> Elm -> Elm
equalsPair symbol forceMultiline left right =
    Fix $ EqualsPair forceMultiline left symbol right


{-|
An equalsPair where the left side is an application
-}
definition :: Text -> Bool -> Elm -> [Elm] -> Elm -> Elm
definition symbol forceMultiline first [] =
    equalsPair symbol forceMultiline first
definition symbol forceMultiline first (arg0:args) =
    equalsPair symbol forceMultiline
        (application (FAJoinFirst JoinAll) first (arg0:|args))


{-|
Formats as:

    first rest0 rest1 rest2

    first rest0
      rest1
      rest2

    first
      rest0
      rest1
      rest2
-}
application :: FunctionApplicationMultiline -> Elm -> NonEmpty Elm -> Elm
application forceMultiline f args =
    Fix $ FunctionApplication forceMultiline f args


unary :: Block.Line -> Elm -> Elm
unary op a =
    Fix $ OperatorPrefix False op a


{-|
Formats as:

    case subject of
        clause0
        clause1

    case
        subject
    of
        clause0
        clause1
-}
case' :: Text -> Text -> Bool -> Elm -> [Elm] -> Elm
case' caseWord ofWord forceMultilineSubect subject clauses =
    Fix $ Case caseWord ofWord forceMultilineSubect subject clauses


{-|
Formats as:

    pattern ->
        body

    pattern
    ->
        body
-}
caseBranch :: Text -> Bool -> Elm -> Elm -> Elm
caseBranch arrow forceArrowNewline pattern body =
    Fix $ CaseClause forceArrowNewline pattern arrow body


{-|
Formats as:

    let
        defs
    in
    body
-}
letIn :: Text -> Text -> Elm -> Elm -> Elm
letIn letWord inWord defs body =
    Fix $ LetIn letWord defs inWord body


{-|
Formats as:

    if condition then
        ifBody
    else if condition1 then
        elseIfBody1
    else
        elseBody
-}
ifElse :: Text -> Text -> Text -> Elm -> Elm -> [(Maybe Elm, Elm, Elm)] -> Elm -> Elm
ifElse ifWord thenWord elseWord condition ifBody elseIfs elseBody =
    Fix $ IfElse ifWord condition thenWord ifBody elseIfs elseWord elseBody


{-|
Formats as:

    \arg0 arg1 arg2 -> body

    \arg0 arg1 arg2 ->
        body
-}
lambda :: Text -> Text -> Bool -> Elm -> Maybe Elm -> Elm -> Elm
lambda start arrow forceMultiline args bodyComments body =
    Fix $ Lambda start arrow forceMultiline args bodyComments body


{-|
`group True '<' ';' '>'` formats as:

    <>

    < item0 >

    < item0; item1; item2 >

    < item0
    ; item1
    ; item2
    >
-}
group :: Bool -> Text -> Text -> Text -> Bool -> [Elm] -> Elm
group innerSpaces left sep right forceMultiline items =
  group' innerSpaces left sep Nothing right forceMultiline items


{-|
Formats like `group` if there is no extraFooter, or as:

    < item0
    ; item1
    ; item2

    extraFooter
    >

-}
group' :: Bool -> Text -> Text -> Maybe Elm -> Text -> Bool -> [Elm] -> Elm
group' _ left _ Nothing right _ [] =
    Fix $ Keyword (left <> right)
group' innerSpaces left _ (Just extraFooter) right _ [] =
    groupOfOne left right extraFooter
group' innerSpaces left sep extraFooter right forceMultiline (first:rest) =
    sectionedGroup innerSpaces left sep right forceMultiline (first:|rest) [] extraFooter


{-|
Formats like `group'` if there are no labelled sections, or as:

    < item0.0
    ; item0.1

    label1
    ; item1.0
    ; item1.1

    extraFooter
    >

-}
sectionedGroup :: Bool -> Text -> Text -> Text -> Bool -> NonEmpty Elm -> [(Elm, NonEmpty Elm)] -> Maybe Elm -> Elm
sectionedGroup innerSpaces left sep right forceMultiline section1 sections extraFooter =
    Fix $ SectionedGroup innerSpaces left sep right forceMultiline section1 sections extraFooter

{-|
Formats as:

    { base | first }

    { base | first, rest0, rest1 }

    { base
      | first
      , rest0
      , rest1
    }
-}
extensionGroup :: Text -> Text -> Text -> Text -> Bool -> Elm -> NonEmpty Elm -> [(Elm, NonEmpty Elm)] -> Maybe Elm -> Elm
extensionGroup left delim sep right forceMultiline base section1 sections extraFooter =
    Fix $ ExtensionGroup left delim sep right forceMultiline base section1 sections extraFooter


{-|
Formats as:

    [a..b]

    [
        a
    ..
        b
    ]
-}
range :: Text -> Text -> Text -> Elm -> Elm -> Elm
range left dots right a b =
    Fix $ Range left dots right a b
