module Main exposing (foo)


foo =
    [ fst ( 1, "Two" )
    , snd ( 2, "One" )
    , f fst snd
    , Basics.fst ( 1, "Two" )
    , Basics.snd ( 1, "Two" )
    , ( f, "x" ) `fst` ()
    , ( f, "x" ) `snd` ()
    , ( f, "x" ) `Basics.fst` ()
    , ( f, "x" ) `Basics.snd` ()
    , Quux.fst ( 1, "Two" )
    , Quux.snd ( 1, "Two" )
    ]
