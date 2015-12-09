module Comments (..) where

{-| An example of all valid Elm syntax.

# Section
@docs fn
-}

-- Comments before imports

import Json.Decode as Json
import List exposing (..)


--
-- Imports starting with S
--

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
        +
            3


comments3 =
    let
        -- The return value
        x = ()

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
        y = ()
    in
        ()


comments8 =
    [ {- A -} 7 {- X -}
    , -- B
      -- C
      8
      -- Y
      -- Z
    ]
