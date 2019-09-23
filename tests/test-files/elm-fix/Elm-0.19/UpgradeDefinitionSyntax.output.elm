module UpgradDefinitionSyntax exposing (x)


x =
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


cases =
    [ 0
    , 0
    , 13
    , 0
    , case Just () of
        Nothing ->
            0

        _ ->
            99
    , "style1"
    ]
