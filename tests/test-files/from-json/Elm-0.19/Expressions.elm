module Expressions exposing (charLiteral, emptyList, floatLiteral, functionApplication, intLiteral, list, operator, parensAreAddedWhereNeeded, stringLiteral, tag, unit, var)


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


functionApplication =
    f (g ())


parensAreAddedWhereNeeded =
    f () ()


emptyList =
    []


list =
    [ ()
    , ()
    ]
