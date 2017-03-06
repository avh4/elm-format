module AllSyntax.LineComments.Expressions exposing (..)


unit =
    (--A
    )


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


functionApplication =
    max 1 2


unary x =
    -x


binary =
    1 + 2 + 3


parens x =
    (x)


emptyList =
    [--B
    ]


list =
    [ (), () ]


tuple =
    ( 1, 2 )


tupleFunction =
    (,,) 1 2 3


emptyRecord =
    { --U
    }


record =
    { x =
        --M
        1
        --N
    , --O
      y = 2
    , --Q
      z
      --R
        =
        --S
        3
        --T
    }


recordUpdate a =
    { a | x = 1, y = 2 }


recordAccess =
    { x = 1 }.x


recordAccessfunction =
    .x { x = 1 }


lambda =
    \x -> x + 1


ifStatement =
    if
        --C
        True
        --D
    then
        --E
        1
        --F
    else
    --G
    if
        --H
        False
        --I
    then
        --J
        2
        --K
    else
        --L
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
