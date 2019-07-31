module CommaFunctionsBecomeLabdas exposing (tuple2)


tuple2 =
    ( 1, 2 )


tuple2_partial =
    \b -> ( 1, b )


tuple2_function =
    \a b -> ( a, b )


tuple3 =
    ( 1, 2, 3 )


tuple3_partial1 =
    \c -> ( 1, 2, c )


tuple3_partial2 =
    \b c -> ( 1, b, c )


tuple3_function =
    \a b c -> ( a, b, c )


bad_tooManyArgs =
    ( 1, 2, 3 ) 4 5


withComments =
    {- A -}
    \c d -> ( {- B -} 1, {- C -} 2, c, d )


needsParens f =
    f (\b c -> ( 1, b, c ))


multiline_splitAll =
    \c ->
        ( { longRecordWithFields = 1, longSecondField = 2 }
        , { longRecordWithFields = 3, longSecondField = 4 }
        , c
        )


multiline_splitSome =
    \c ->
        ( { longRecordWithFields = 1, longSecondField = 2 }
        , { longRecordWithFields = 3, longSecondField = 4 }
        , c
        )


multiline_splitFirst =
    \b c d ->
        ( { longRecordWithFields = 1, longSecondField = 2 }
        , b
        , c
        , d
        )


multiline_fromComments =
    \c ->
        ( 1
        , --A
          2
        , c
        )


notMultiline_joinFirst =
    \b c d -> ( { longRecordWithFields = 1, longSecondField = 2 }, b, c, d )
