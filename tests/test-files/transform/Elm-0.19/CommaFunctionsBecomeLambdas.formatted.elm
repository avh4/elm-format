module Main exposing (..)


tuple2 =
    (\a b -> ( a, b )) 1 2


tuple2_partial =
    (\a b -> ( a, b )) 1


tuple2_function =
    \a b -> ( a, b )


tuple3 =
    (\a b c -> ( a, b, c )) 1 2 3


tuple3_partial1 =
    (\a b c -> ( a, b, c )) 1 2


tuple3_partial2 =
    (\a b c -> ( a, b, c )) 1


tuple3_function =
    \a b c -> ( a, b, c )
