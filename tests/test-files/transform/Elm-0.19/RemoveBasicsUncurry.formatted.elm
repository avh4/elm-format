module Main exposing (extraArgs, extraArgs_notTuple, fullyApplied, fullyApplied_comments, functionOnly, functionOnly_comments, multiline_extraArgs_notTuple, multiline_fullyApplied, nested, notTuple, notTuple_comments, qualified, unapplied)


fullyApplied f =
    f "1" 2


fullyApplied_comments f =
    ({- A -} f) {- B -} ({- C -} "1" {- D -}) ({- E -} 2 {- F -})


notTuple f t =
    (\( a, b ) -> f a b) t


notTuple_comments f t =
    (\( a, b ) -> ({- A -} f) a b) {- B -} t


functionOnly f =
    \( a, b ) -> f a b


functionOnly_comments f =
    \( a, b ) -> ({- A -} f) a b


unapplied =
    \f ( a, b ) -> f a b


extraArgs f =
    f 1 2 3 4 5


extraArgs_notTuple f t u v =
    (\( a, b ) -> f a b) t u v


qualified f =
    f "1" 2


nested f =
    f (f 1 2) 3


multiline_fullyApplied f =
    f 1 2


multiline_extraArgs_notTuple f t u =
    (\( a, b ) -> f a b)
        t
        u
