module AllSyntax where

import String
import Signal exposing (foldp, map)
import Json.Decode as Json
import List exposing (..)


fn =
    "XYZZY"


annotatedFn : String
annotatedFn =
    "XYZZY"


inlinePipeline =
    1 |> (+) 2


commentedLiterals =
    {- one -} 1 + {- two -} 2
