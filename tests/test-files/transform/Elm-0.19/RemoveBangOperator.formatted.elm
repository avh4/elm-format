module Main exposing (bangFunction, bangFunction_extraArgs, bangFunction_partiallyApplied, bangFunction_unapplied, batch, batch_notALiteralList, batch_withComments, doesntMessUpOtherBinops, inBinaryExpression, multipleBangs, nestedBangs, none, none_withComments, single, single_withComments, withFunctionApplication, withHighPrecedenceOperators_leftAssoc, withHighPrecedenceOperators_leftAssoc_complex, withHighPrecedenceOperators_rightAssoc)


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


single model a =
    ( model
    , a
    )


single_withComments model a =
    ( model {- A -}
    , {- B -} {- C -} a {- D -}
    )


inBinaryExpression model x a b =
    x
        + ( model
          , a
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
        , a
        )
      , b
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


doesntMessUpOtherBinops f g h =
    ( f >> g >> h, f << g << h )


bangFunction model =
    ( model
    , Cmd.batch []
    )


bangFunction_unapplied =
    \model cmds ->
        ( model
        , Cmd.batch cmds
        )


bangFunction_partiallyApplied model =
    \cmds ->
        ( model
        , Cmd.batch cmds
        )


bangFunction_extraArgs model =
    (( model
     , Cmd.batch []
     )
    )
        ()
