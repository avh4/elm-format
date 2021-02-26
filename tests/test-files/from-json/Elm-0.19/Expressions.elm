module Expressions exposing (charLiteral, emptyList, externalReference, floatLiteral, functionApplication, intLiteral, list, operator, parensAreAddedWhereNeeded, record, recordAccessFunction, recordUpdate, stringLiteral, tag, tuple, unary, unit, var)


unit =
    ()


intLiteral =
    1


floatLiteral =
    2.0


stringLiteral =
    "string"


charLiteral =
    'c'


var =
    x


operator =
    (+)


tag =
    MyTag


externalReference =
    List.map


functionApplication =
    f (g ())


parensAreAddedWhereNeeded =
    f () ()


unary =
    -x


emptyList =
    []


list =
    [ ()
    , ()
    ]


tuple =
    ( ()
    , ()
    )


record =
    { x = ()
    , y = ()
    }


recordUpdate =
    { r
        | x = ()
        , y = ()
    }


recordAccessFunction =
    .x
