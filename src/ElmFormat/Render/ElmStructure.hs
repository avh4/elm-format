{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.ElmStructure
  ( spaceSepOrStack, spaceSepOrIndented, forceableSpaceSepOrIndented, spaceSepOrPrefix
  , equalsPair, definition
  , application, group, extensionGroup )
  where


import Elm.Utils ((|>))
import Box
import AST.V0_16 (FunctionApplicationMultiline(..), Multiline(..))

import qualified Data.List as List

{-|
Formats as:

    first rest0 rest1

    first
    rest0
    rest1
-}
spaceSepOrStack :: Box -> [Box] -> Box
spaceSepOrStack first rest =
    case
      ( first, allSingles rest )
    of
      ( SingleLine first', Right rest' ) ->
        line $ row $ List.intersperse space (first' : rest')

      _ ->
        stack1 (first : rest)

{-|
Formats as:

    first rest0 rest1 rest2

    first
      rest0
      rest1
      rest2
-}
spaceSepOrIndented :: Box -> [Box] -> Box
spaceSepOrIndented =
    forceableSpaceSepOrIndented False


forceableSpaceSepOrIndented :: Bool -> Box -> [Box] -> Box
forceableSpaceSepOrIndented forceMultiline first rest =
  case
    ( forceMultiline, first, allSingles rest )
  of
    ( False, SingleLine first', Right rest' ) ->
      line $ row $ List.intersperse space (first' : rest')

    _ ->
      stack1
        ( first : map indent rest)


{-|
Formats as:

    op rest

    op rest1
       rest2

    opLong
        rest
-}
spaceSepOrPrefix :: Box -> Box -> Box
spaceSepOrPrefix op rest =
    case ( op, rest) of
        ( SingleLine op', SingleLine rest' ) ->
            line $ row [ op', space, rest' ]

        ( SingleLine op', _ ) | lineLength 0 op' < 4 ->
            prefix (row [ op', space ]) rest

        _ ->
            stack1 [ op, indent rest ]


{-|
Formats as:

    left = right

    left =
      right

    left
      =
      right
-}
equalsPair :: String -> Bool -> Box -> Box -> Box
equalsPair symbol forceMultiline left right =
  case (forceMultiline, left, right) of
    ( False, SingleLine left', SingleLine right' ) ->
      line $ row
        [ left'
        , space
        , punc symbol
        , space
        , right'
        ]

    ( _, SingleLine left', MustBreak right' ) ->
      mustBreak $ row
        [ left'
        , space
        , punc symbol
        , space
        , right'
        ]

    ( _, SingleLine left', right' ) ->
      stack1
        [ line $ row [ left', space, punc symbol ]
        , indent right'
        ]

    ( _, left', right' ) ->
      stack1
        [ left'
        , indent $ line $ punc symbol
        , indent right'
        ]


{-|
An equalsPair where the left side is an application
-}
definition :: String -> Bool -> Box -> [Box] -> Box -> Box
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
application :: FunctionApplicationMultiline -> Box -> [Box] -> Box
application forceMultiline first args =
  case args of
    [] ->
      first

    arg0 : rest ->
      case
        ( forceMultiline
        , first
        , arg0
        , allSingles rest
        )
      of
        ( FAJoinFirst JoinAll, SingleLine first', SingleLine arg0', Right rest' ) ->
          (first' : arg0' : rest' )
            |> List.intersperse space
            |> row
            |> line

        ( FAJoinFirst _, SingleLine first', SingleLine arg0', _) ->
          stack1
            $ line ( row [ first', space, arg0' ])
              : map indent rest

        _ ->
          stack1
            $ first : map indent (arg0 : rest)

{-|
`group True '<' ',' '>'` formats as:

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
  case (forceMultiline, allSingles children) of
    (_, Right []) ->
      line $ row [punc left, punc right]

    (False, Right ls) ->
      line $ row $ concat
        [ if innerSpaces then [punc left, space] else [punc left]
        , List.intersperse (row [punc sep, space]) ls
        , if innerSpaces then [space, punc right] else [punc right]
        ]

    _ ->
      case children of
        [] ->
          line $ row [ punc left, punc right]

        (first:rest) ->
          stack1 $
            prefix (row [punc left, space]) first
            : map (prefix $ row [punc sep, space]) rest
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
