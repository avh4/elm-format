module RemoveBasicsCurry exposing (fullyApplied)


fullyApplied f =
    f ( "1", 2 )


fullyApplied_comments f =
    ({- A -} f) ( {- B -} "1", {- C -} 2 )


partiallyApplied f =
    \b -> f ( "1", b )


partiallyApplied_comments f =
    \b -> ({- A -} f) ( {- B -} "1", b )


functionOnly f =
    \a b -> f ( a, b )


functionOnly_comments f =
    \a b -> ({- A -} f) ( a, b )


unapplied =
    \f a b -> f ( a, b )


extraArgs f =
    f ( 1, 2 ) 3 4 5


qualified f =
    f ( "1", 2 )


nested f =
    f ( f ( 1, 2 ), 3 )
