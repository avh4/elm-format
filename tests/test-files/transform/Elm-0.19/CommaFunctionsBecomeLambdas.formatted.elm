module Main exposing (..)


tuple2 =
    (\a b -> ( a, b )) 1 2


tuple2_partial =
    (\a b -> ( a, b )) 1


tuple2_function =
    \a b -> ( a, b )
