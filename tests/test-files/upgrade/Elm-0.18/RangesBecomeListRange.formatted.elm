module RangesBecomeListRange exposing (range)


range =
    List.range 1 9


variableRange x y =
    List.range x.min y


precommented =
    List.range {- A -} 1 {- B -} 2


precommented2 =
    List.range
        -- One
        1
        -- Two
        2


commented =
    List.range ({- A -} 1 {- B -}) ({- C -} 2 {- D -})


needsParens =
    List.range (f x) (f y)


usedInFunctionCall =
    f (List.range 1 9) (List.range ({- A -} 1 {- B -}) ({- C -} 2 {- D -}))
