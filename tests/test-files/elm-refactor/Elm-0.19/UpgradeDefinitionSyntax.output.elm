module UpgradDefinitionSyntax exposing (x)

import B


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
    , ()
    ]


tags =
    [ ()
    , \a b -> a + b
    , \b -> 100 + b
    , 101 + 102
    , (103 + 104) 105
    ]


cases s =
    [ 0
    , 0
    , 13
    , (+) 14 15
    , 16 + 17
    , identity <| 18 + 19
    , 0
    , 99
    , "style1"
    , ElmFix.todo UnknownStyle
    , case s of
        A.Style1 ->
            "style1"

        _ ->
            ElmFix.todo s
    ]


typeChange :
    A.NewType String Int
    -> B.NewType ( {}, Never, Never )
    -> ()
typeChange _ _ =
    ()
