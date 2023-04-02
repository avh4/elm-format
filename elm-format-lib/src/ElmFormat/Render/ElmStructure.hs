{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.ElmStructure
  ( spaceSepOrStack, forceableSpaceSepOrStack, forceableSpaceSepOrStack1
  , forceableRowOrStack
  , spaceSepOrIndented, forceableSpaceSepOrIndented, spaceSepOrPrefix, prefixOrIndented
  , equalsPair, definition
  , application, group, group', extensionGroup, extensionGroup' )
  where


import Elm.Utils ((|>))
import Box hiding (Line, space)
import AST.V0_16 (FunctionApplicationMultiline(..), Multiline(..))
import Box.BlockAdapter (Block, space, Line)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List as List
import qualified Box.BlockAdapter as Block


{-| Same as `forceableSpaceSepOrStack False`
-}
spaceSepOrStack :: Block -> [Block] -> Block
spaceSepOrStack =
    forceableSpaceSepOrStack False


{-|
Formats as:

    first rest0 rest1

    first
    rest0
    rest1
-}
forceableSpaceSepOrStack :: Bool -> Block -> [Block] -> Block
forceableSpaceSepOrStack forceMultiline first rest =
    Block.rowOrStackForce forceMultiline (Just space) (first :| rest)


forceableRowOrStack :: Bool -> Block -> [Block] -> Block
forceableRowOrStack forceMultiline first rest =
    Block.rowOrStackForce forceMultiline Nothing (first :| rest)


{-| Same as `forceableSpaceSepOrStack`
-}
forceableSpaceSepOrStack1 :: Bool -> [Block] -> Block
forceableSpaceSepOrStack1 forceMultiline boxes =
    case boxes of
        (first:rest) ->
            forceableSpaceSepOrStack forceMultiline first rest

        _ ->
            error "forceableSpaceSepOrStack1 with empty list"


{-|
Formats as:

    first rest0 rest1 rest2

    first
      rest0
      rest1
      rest2
-}
spaceSepOrIndented :: Block -> [Block] -> Block
spaceSepOrIndented =
    forceableSpaceSepOrIndented False


forceableSpaceSepOrIndented :: Bool -> Block -> [Block] -> Block
forceableSpaceSepOrIndented forceMultiline first rest =
    Block.rowOrIndentForce forceMultiline (Just space) (first :| rest)


{-|
Formats as:

    op rest

    op rest1
       rest2

    opLong
        rest
-}
spaceSepOrPrefix :: Word -> Line -> Block -> Block
spaceSepOrPrefix opLen op rest =
    if opLen >= 4
        then Block.rowOrIndent (Just space) (Block.line op :| [ rest ])
        else Block.prefix opLen (op <> space) rest


prefixOrIndented :: Line -> Block -> Block
prefixOrIndented =
    Block.prefixOrIndent (Just space)


{-|
Formats as:

    left = right

    left =
      right

    left
      =
      right
-}
equalsPair :: String -> Bool -> Block -> Block -> Block
equalsPair symbol forceMultiline left right =
    Block.rowOrIndentForce forceMultiline (Just space)
      [ Block.rowOrIndent (Just space) [ left, line $ punc symbol ]
      , right
      ]


{-|
An equalsPair where the left side is an application
-}
definition :: String -> Bool -> Block -> [Block] -> Block -> Block
definition symbol forceMultiline first rest =
  equalsPair symbol forceMultiline
    (application (FAJoinFirst JoinAll) first rest)


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
application :: FunctionApplicationMultiline -> Block -> [Block] -> Block
application forceMultiline first args =
  case args of
    [] ->
      first

    arg0 : rest ->
      Block.rowOrIndentForce splitRest (Just space) $
          Block.rowOrIndentForce splitFirst (Just space) [ first, arg0 ]
          :| rest
      where
        (splitFirst, splitRest) =
            case forceMultiline of
                FASplitFirst -> (True, True)
                FAJoinFirst forceRest ->
                    ( False
                    , case forceRest of
                        SplitAll -> True
                        JoinAll -> False
                    )


{-|
`group True '<' ';' '>'` formats as:

    <>

    < child0 >

    < child0; child1; child2 >

    < child0
    ; child1
    ; child2
    >
-}
group :: Bool -> String -> String -> String -> Bool -> [Box] -> Box
group innerSpaces left sep right forceMultiline children =
  group' innerSpaces left sep [] right forceMultiline children


group' :: Bool -> String -> String -> [Box] -> String -> Bool -> [Box] -> Box
group' innerSpaces left sep extraFooter right forceMultiline children =
  case (forceMultiline, allSingles children, allSingles extraFooter) of
    (_, Right [], Right efs) ->
      line $ row $ concat @[] [[punc left], efs, [punc right]]

    (False, Right ls, Right efs) ->
      line $ row $ concat @[]
        [ if innerSpaces then [punc left, space] else [punc left]
        , List.intersperse (row [punc sep, space]) (ls ++ efs)
        , if innerSpaces then [space, punc right] else [punc right]
        ]

    _ ->
      case children of
        [] ->
          -- TODO: might lose extraFooter in this case, but can that ever happen?
          line $ row [ punc left, punc right]

        (first:rest) ->
          stack1 $
            prefix (row [punc left, space]) first
            : map (prefix $ row [punc sep, space]) rest
            ++ extraFooter
            ++ [ line $ punc right ]

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
extensionGroup :: Bool -> Box -> Box -> [Box] -> Box
extensionGroup multiline base first rest =
  case
    ( multiline
    , isLine base
    , allSingles (first : rest)
    )
  of
    (False, Right base', Right fields') ->
      line $ row
        [ punc "{"
        , space
        , base'
        , space
        , punc "|"
        , space
        , row (List.intersperse (row [punc ",", space]) fields')
        , space
        , punc "}"
        ]

    _ ->
      stack1
        [ prefix (row [punc "{", space]) base
        , stack1
            ( prefix (row [punc "|", space]) first
            : map (prefix (row [punc ",", space])) rest)
            |> indent
        , line $ punc "}"
        ]


extensionGroup' :: Bool -> Box -> Box -> Box
extensionGroup' multiline base fields =
  case
    ( multiline
    , base
    , fields
    )
  of
    (False, SingleLine base', SingleLine fields') ->
      line $ row $ List.intersperse space
        [ punc "{"
        , base'
        , fields'
        , punc "}"
        ]

    _ ->
      stack1
        [ prefix (row [punc "{", space]) base
        , indent fields
        , line $ punc "}"
        ]
