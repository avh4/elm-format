{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.ElmStructure
  ( spaceSepOrStack, spaceSepOrStackForce
  , rowOrStackForce
  , spaceSepOrIndented, spaceSepOrIndentedForce, spaceSepOrPrefix, prefixOrIndented
  , equalsPair, definition
  , application, group, group', extensionGroup' )
  where


import AST.V0_16 (FunctionApplicationMultiline(..), Multiline(..))
import Box.BlockAdapter (Block, space, Line)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Box.BlockAdapter as Block


{-| Same as `spaceSepOrStackForce False`
-}
spaceSepOrStack :: NonEmpty Block -> Block
spaceSepOrStack =
    spaceSepOrStackForce False


{-|
Formats as:

    first rest0 rest1

    first
    rest0
    rest1
-}
spaceSepOrStackForce :: Bool -> NonEmpty Block -> Block
spaceSepOrStackForce forceMultiline =
    Block.rowOrStackForce forceMultiline (Just space)


rowOrStackForce :: Bool -> NonEmpty Block -> Block
rowOrStackForce forceMultiline =
    Block.rowOrStackForce forceMultiline Nothing


{-|
Formats as:

    first rest0 rest1 rest2

    first
      rest0
      rest1
      rest2
-}
spaceSepOrIndented :: NonEmpty Block -> Block
spaceSepOrIndented =
    spaceSepOrIndentedForce False


spaceSepOrIndentedForce :: Bool -> NonEmpty Block -> Block
spaceSepOrIndentedForce forceMultiline =
    Block.rowOrIndentForce forceMultiline (Just space)


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
      [ Block.rowOrIndent (Just space) [ left, Block.line $ Block.string7 symbol ]
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
group :: Bool -> String -> String -> String -> Bool -> [Block] -> Block
group innerSpaces left sep right forceMultiline children =
  group' innerSpaces left sep [] right forceMultiline children


group' :: Bool -> String -> String -> [Block] -> String -> Bool -> [Block] -> Block
group' _ open _ [] close _ [] =
  Block.line $ Block.string7 open <> Block.string7 close
group' innerSpaces open _ (extraFooter0 : extraFooter) close forceMultiline [] =
  Block.rowOrStackForce
    forceMultiline
    (if innerSpaces then Just Block.space else Nothing)
    [ Block.rowOrStackForce forceMultiline Nothing $
        formatStart extraFooter0
          :| extraFooter,
      Block.line (Block.string7 close)
    ]
  where
    formatStart =
      if innerSpaces || forceMultiline
        then Block.prefix 2 (Block.string7 open <> space)
        else Block.prefix 1 (Block.string7 open)
group' innerSpaces open sep extraFooter close forceMultiline (first : rest) =
  Block.rowOrStackForce
    forceMultiline
    (if innerSpaces then Just Block.space else Nothing)
    [ Block.rowOrStackForce forceMultiline Nothing $
        formatEntry open (innerSpaces || forceMultiline) first
          :| fmap (formatEntry sep True) rest
          <> extraFooter,
      Block.line (Block.string7 close)
    ]
  where
    formatEntry char useSpace entry =
      if useSpace
        then Block.prefix 2 (Block.string7 char <> space) entry
        else Block.prefix 1 (Block.string7 char) entry

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
extensionGroup' :: Char -> Char -> Bool -> Block -> Block -> Block
extensionGroup' open close multiline base fields =
      Block.rowOrStackForce multiline (Just space)
        [ Block.rowOrIndentForce multiline (Just space)
            [ Block.prefix 2 (Block.char7 open <> Block.space) base,
              fields
            ],
          Block.line $ Block.char7 close
        ]
