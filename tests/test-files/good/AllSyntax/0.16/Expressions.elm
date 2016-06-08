module AllSyntax.Expressions (..) where


unit =
    ()


literal =
    ( 1
    , 2.0
    , "string"
    , 'c'
    )


var x =
    x


operator =
    (+)


infixApplication =
    1 `operator` 2


infixApplicationMultiline =
    x
        `infixOperator`
            [ y
            , z
            ]


functionApplication =
    max 1 2


functionApplicationMultiline =
    max 1
        2


unary x =
    -x


binary =
    1 + 2 + 3


parens x =
    (x)


emptyList =
    []


list =
    [ (), () ]


range =
    [1..2]


tuple =
    ( 1, 2 )


tupleFunction =
    (,,) 1 2 3


emptyRecord =
    {}


record =
    { x = 1, y = 2 }


recordUpdate a =
    { a | x = 1, y = 2 }


recordAccess =
    { x = 1 }.x


recordAccessfunction =
    .x { x = 1 }


lambda =
    \x -> x + 1


ifStatement =
    if True then
        1
    else if False then
        2
    else
        3


letStatement =
    let
        x =
            1

        y =
            2
    in
        ( x, y )


caseStatement =
    case Just 1 of
        Just x ->
            x

        _ ->
            2
