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


tuple =
    (1, 2)


commentedLiterals =
    ({- int -} 1, {- float -} 0.1, {- char -} 'c', {- string -} "str", {- boolean -} True)


infixOperator =
    1 + 2 * 3 / 4 // 5 |> (+) 0


functionWithParam a =
    a


functionParameters a (t,s,_,(t',{x',y'})) {x,y} _ =
    ()


fnAsLambda =
    (\a -> a)


fnAsUnparenthesizedLambda =
    \arg -> arg


multiArgLambda =
    \a b (t,s) {x,y} _ -> \c -> (\d -> ())


parenthesizedExpressions =
    (1 + (2 * 3) / 4) |> ((+) 0)
