module AllSyntax.Module (a, b, c) where

import Dict as D
    exposing
        ( empty
        , fromList
        )
import Json.Decode as Json
import Maybe exposing (Maybe(Just, Nothing), map)
import Signal exposing (..)
import String
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
