module AllSyntax.Module (a, b, c) where

import String
import Maybe exposing (Maybe(Just, Nothing), map)
import Json.Decode as Json
import Signal exposing (..)
import Task
    exposing
        ( andMap
        , andThen
        , fail
        , fromMaybe
        , fromResult
        , map
        , map2
        , map3
        , map4
        , map5
        , mapError
        , onError
        , succeed
        , toMaybe
        , toResult
        )
import Dict as D
    exposing
        ( empty
        , fromList
        )


a =
    1


b =
    2


c =
    3
