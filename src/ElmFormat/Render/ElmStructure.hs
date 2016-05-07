{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.ElmStructure
  ( application, group, extensionGroup )
  where


import Elm.Utils ((|>))
import Box

import qualified Data.List as List

{-|
Formats as:

    first rest0 rest1 rest2

    first
      rest0
      rest1
      rest2
-}
application :: Bool -> Box -> [Box] -> Box
application forceMultiline first rest =
  case
    ( forceMultiline
    , allSingles (first:rest)
    )
  of
    ( False, Right ls ) ->
      ls
        |> List.intersperse space
        |> row
        |> line

    _ ->
      stack1
        $ first : (map indent rest)

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
            (prefix (row [punc left, space]) first)
            : (map (prefix $ row [punc sep, space]) rest)
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
    , allSingles $ (first:rest)
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
            ([ prefix (row [punc "|", space]) first ]
            ++ (map (prefix (row [punc ",", space])) rest))
            |> indent
        , line $ punc "}"
        ]
