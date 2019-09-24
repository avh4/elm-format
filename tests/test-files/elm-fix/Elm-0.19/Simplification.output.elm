module Simplification exposing (x)


x a1 =
    [ 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , case a1 of
        A.First ->
            10

        A.Second ->
            11
    ]
