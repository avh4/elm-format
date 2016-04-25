module AllSyntax.Module (a, b, c) where

import String
import Maybe exposing (Maybe(Just, Nothing), map)
import Json.Decode as Json
import Signal exposing (..)
import Task
  exposing
    ( succeed
    , fail
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


a =
  1


b =
  2


c =
  3
