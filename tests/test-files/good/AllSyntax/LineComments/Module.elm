--AD


module
  --A
  AllSyntax.LineComments.Module
  --B
  ( --C
    a
    --D
  , --E
    b
    --F
  , --G
    c
    --H
  )
  --I
  where

--J

import
  --L
  String


--K

import
  --M
  Maybe
    --N
    exposing
      --O
      ( --S
        Maybe
        --W
          ( --X
            Just
            --Y
          , --Z
            Nothing
            --AA
          )
        --T
      , --U
        map
        --V
      )
import
  --P
  Json.Decode
    --Q
    as
      --R
      Json
import Signal
  exposing
    (--AB
     ..
     --AC
    )
import Task
  exposing
    ( succeed
      -- AD
    , fail
      -- AE
    , map
    , map2
    , map3
    , map4
    , map5
    , andMap
    , andThen
    , onError
    , mapError
    , toMaybe
    , fromMaybe
    , toResult
    , fromResult
    )


-- AF


a =
  1


b =
  2


c =
  3
