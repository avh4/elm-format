module Main exposing (extraArgs, fullyApplied, fullyApplied_comments, functionOnly, functionOnly_comments, nested, partiallyApplied, partiallyApplied_comments, qualified, unapplied)


fullyApplied f =
    f 1 "2"


fullyApplied_comments f =
    ({- A -} f) {- C -} 1 {- B -} "2"


partiallyApplied f =
    \a -> f a "2"


partiallyApplied_comments f =
    \a -> ({- A -} f) a {- B -} "2"


functionOnly f =
    \b a -> f a b


functionOnly_comments f =
    \b a -> ({- A -} f) a b


unapplied =
    \f b a -> f a b


extraArgs f =
    f 2 1 3 4 5


qualified f =
    f 1 "2"


nested f =
    f 3 (f 2 1)
