module Comments exposing (fn)

{-| An example of all valid Elm syntax.


# Section

@docs fn

-}

-- Comments before imports
--
-- Imports starting with S
--

import Json.Decode as Json
import List exposing (..)
import Signal exposing (foldp, map)
import String



----------------
--- Comments ---
----------------


comments1 =
    -- comments inside declaration
    ()


comments2 =
    1
        -- plus
        -- plus
        + 3


records =
    { -- before a field
      f1 = ()
    , f2
      -- after a field
        =
        ()
    , f3 = ()
    }


comments3 =
    let
        -- The return value
        x =
            ()

        {- comment after definitions -}
        -- ...
    in
    {- let body -}
    x


comments4 bool =
    case bool of
        -- This case is for True
        True ->
            -- return unit
            ()

        {- Here's a case for anything else -}
        _ ->
            {- return unit -}
            ()


comments5 =
    if True then
        -- do the right thing
        ()

    else if False then
        -- do something
        -- redundant
        ()

    else
        {- do the wrong thing -}
        ()


comments6 =
    \x ->
        -- we compute using x
        x * x


comments7 =
    let
        x =
            case True of
                _ ->
                    ()

        -- comments
        y =
            ()
    in
    ()


comments8 =
    [ {- A -} 7

    {- X -}
    , -- B
      -- C
      8

    -- Y
    -- Z
    ]


expressionDefinition {- F -} _ {- G -} _ =
    {- H -}
    {- I -}
    ()


infix {- A -} 7 {- B -} ===


(===) =
    always


port {- A -} runner {- B -} : {- C -} Signal (Task.Task x ())
port {- A -} runner {- B -} =
    -- C
    Signal.constant (Task.succeed ())


blockCommentWithOnlyWhitespaceLines =
    {-


    -}
    ()
