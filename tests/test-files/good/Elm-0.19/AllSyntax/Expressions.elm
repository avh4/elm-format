module AllSyntax.Expressions exposing (unit)

import Foo


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


infixPipeLeft x y z =
    x <|
        [ y
        , z
        ]


infixPipeLeft2 x y z =
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


parens f x a =
    f (x a)


emptyList =
    [ []
    , [{- A -}]
    , [--B
      ]
    ]


list =
    [ [ (), () ]
    , [ {- A -} ()

      {- B -}
      , {- C -} ()

      {- D -}
      ]
    , [ --A
        ()

      --B
      , --C
        ()

      --D
      ]
    , [ () --A
      , () --B
      ]
    ]


listWithCommentedOutItems =
    [ 1

    -- , 2
    -- , 3
    , 4

    -- , 5
    , 6
    ]


tuple =
    [ ( 1, 2 )
    , ( {- A -} 1 {- B -}, {- C -} 2 {- D -} )
    , ( --A
        1
        --B
      , --C
        2
        --D
      )
    , ( 1
        --A
      , 2
        --B
      )
    ]


tupleWithCommentedOutItems =
    ( 1
      -- , 2
      -- , 3
    , 4
      -- , 5
    , 6
    )


tupleFunction =
    (,,) 1 2 3


emptyRecord =
    [ {}
    , {{- A -}}
    , { --B
      }
    ]


record =
    ( { x = 1 }
    , { x = 1, y = 2 }
    , { x = 1, y = 2, z = 3 }
    , { x = 1
      }
    , { x = 1
      , y = 2
      }
    , { {- A -} x {- B -} = {- C -} 1

      {- D -}
      , {- E -} y {- F -} = {- G -} 2

      {- H -}
      , {- I -} z {- J -} = {- K -} 3

      {- L -}
      }
    , { x =
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
    , { x = 1 --X
      , y = 2 --Y
      , z = 3 --Z
      }
    , { -- comment about x
        x = 1
      , y = 2

      -- Section 2
      , z = 3
      }
    )


recordWithCommentedOutFields =
    { x = 1

    -- , y = 2
    -- , z = 3
    , a = 4

    -- , b = 5
    }


recordUpdate a =
    ( { a | x = 1 }
    , { a | x = 1, y = 2 }
    , { a
        | x = 1
      }
    , { a
        | x = 1
        , y = 2
      }
    )


recordUpdateWithCommentedOutFields a =
    { a
        | x = 1

        -- , y = 2
        -- , z = 3
        , a = 4

        -- , b = 5
    }


recordAccess f a b r =
    ( ().x
    , a.x
    , (f a).x
    , ( 1, 2 ).x
    , (,,).x
    , {}.x
    , { x = 1 }.x
    , { r | x = 1 }.x
    , a.x.y
    , Foo.record.x
    )


recordAccessfunction =
    .x { x = 1 }


lambda =
    \x -> x + 1


lambdaWithMultilinePattern =
    \{ x
     , --A
       y
     }
    ->
        ()


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
    let
        a =
            case Just 1 of
                Just x ->
                    x

                _ ->
                    2

        b =
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

        c =
            case
                --M
                Just 1
                --N
            of
                --O
                Just x
                --P
                ->
                    --Q
                    x

                --R
                _
                --S
                ->
                    --T
                    2
    in
    {}
