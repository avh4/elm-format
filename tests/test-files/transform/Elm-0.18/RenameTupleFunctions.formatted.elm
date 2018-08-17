module Main exposing (foo)


foo =
    [ Tuple.first ( 1, "Two" )
    , Tuple.second ( 2, "One" )
    , f Tuple.first Tuple.second
    , Tuple.first ( 1, "Two" )
    , Tuple.second ( 1, "Two" )
    , Tuple.first ( f, "x" ) ()
    , Tuple.second ( f, "x" ) ()
    , Tuple.first ( f, "x" ) ()
    , Tuple.second ( f, "x" ) ()
    , Quux.fst ( 1, "Two" )
    , Quux.snd ( 1, "Two" )
    ]
