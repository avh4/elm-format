module AllSyntax.Expressions exposing (..)


unit =
    [ ()
    , ({- A -})
    , (--B
      )
    ]


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


infixPipeLeft =
    x <|
        [ y
        , z
        ]


infixPipeLeft2 =
    [ x
    ]
    <|
        [ y
        , z
        ]


functionApplication =
    max 1 2


functionApplicationMultilineWithFirstArg =
    max 1
        2


functionApplicationMultiline =
    max
        1
        2


unary x =
    -x


binary =
    1 + 2 + 3


parens x =
    (x)


emptyList =
    [ []
    , [{- A -}]
    , [--B
      ]
    ]


list =
    [ (), () ]


tuple =
    ( 1, 2 )


tupleFunction =
    (,,) 1 2 3


emptyRecord =
    [ {}
    , {{- A -}}
    , { --B
      }
    ]


record =
    { x = 1, y = 2 }


recordUpdate a =
    { a | x = 1, y = 2 }


recordAccess f a b r =
    [ ().x
    , a.x
    , (a).x
    , ( 1, 2 ).x
    , (,,).x
    , {}.x
    , { x = 1 }.x
    , { r | x = 1 }.x
    , a.x.y
    ]


recordAccessfunction =
    .x { x = 1 }


lambda =
    \x -> x + 1


ifStatement =
    let
        a =
            if True then
                1
            else if False then
                2
            else
                3

        b =
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

        c =
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
    in
        {}


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
