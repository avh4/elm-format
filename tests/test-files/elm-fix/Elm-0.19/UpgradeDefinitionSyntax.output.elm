module UpgradDefinitionSyntax exposing (x)


x =
    ()


values =
    [ ()
    , \a b -> a + b
    , \b -> 1 + b
    , 2 + 3
    , (4 + 5) 6
    , \a b -> a + b
    , \b -> 7 + b
    , 8 + 9
    , (10 + 11) 12
    ]


tags =
    [ ()
    , \a b -> a + b
    , \b -> 100 + b
    , 101 + 102
    , (103 + 104) 105
    ]


cases =
    [ 0
    , 0
    , 13
    , (+) 14 15
    , 16 + 17
    , identity <| 18 + 19
    , 0
    , case Just () of
        Nothing ->
            0

        _ ->
            99
    , "style1"
    ]
