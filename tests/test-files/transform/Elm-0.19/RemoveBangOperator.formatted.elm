module Main exposing (..)


batch model a b =
    ( model
    , Cmd.batch [ a, b ]
    )


batch_withComments model a b =
    ( model {- A -}
    , Cmd.batch {- B -} [ a, b ]
    )


batch_notALiteralList model cmds =
    ( model
    , Cmd.batch cmds
    )


none model =
    ( model
    , Cmd.none
    )


none_withComments model =
    ( model {- A -}
    , {- B -} Cmd.none {- C -}
    )


inBinaryExpression model x a b =
    x
        + ( model
          , Cmd.batch [ a ]
          )
        * [ b ]


multipleBangs model x =
    x
        + ( model
          , Cmd.none
          )
        * ( model
          , Cmd.none
          )


nestedBangs model a b =
    ( ( ( model
        , Cmd.batch [ a ]
        )
      , Cmd.batch [ b ]
      )
    , Cmd.none
    )


withHighPrecedenceOperators_leftAssoc f g x =
    ( f >> g
    , Cmd.none
    )
        >> x


withHighPrecedenceOperators_leftAssoc_complex a b c d e f g h i j =
    a >> b
        + ( c >> d >> e
          , Cmd.batch f
          )
        >> g
        >> h
        + i
        >> j


withHighPrecedenceOperators_rightAssoc f g x =
    f
        << ( g
           , Cmd.none
           )
        << x


withFunctionApplication f g x =
    ( f x
    , Cmd.batch (g x)
    )
