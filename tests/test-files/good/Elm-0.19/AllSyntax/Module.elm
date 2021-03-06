module AllSyntax.Module exposing (CustomType(..), a, b, c)

import Dict as D
    exposing
        ( Dict
        , empty
        , fromList
        )
import Json.Decode as Json
import Maybe exposing (Maybe(..), map)
import Result exposing (Result(..))
import Signal exposing (..)
import String
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


type CustomType
    = TagA
    | TagB


a =
    1


b =
    2


c =
    3
