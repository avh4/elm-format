module AllSyntax.BlockComments.Expressions (..) where


unit =
    ({- A -})


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


functionApplication =
    max 1 2


unary x =
    -x


binary =
    1 + 2 + 3


parens x =
    (x)


emptyList =
    [{- B -}]


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
    if {- C -} True {- D -} then
        {- E -}
        1
        {- F -}
    else {- G -} if {- H -} False {- I -} then
        {- J -}
        2
        {- K -}
    else
        {- L -}
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
    case {- M -} Just 1 {- N -} of
        {- O -}
        Just x
        {- P -}
        ->
            {- Q -}
            x

        {- R -}
        _
        {- S -}
        ->
            {- T -}
            2
