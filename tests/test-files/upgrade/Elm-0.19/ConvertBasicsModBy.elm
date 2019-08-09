module ConvertBasicsModBy exposing (binop)


binop =
    -1 % 4


binop_comments =
    -1 {- A -} % {- B -} 4


tigherBinding =
    1 ^ 2 % 3 ^ 4


tigherBinding_comments =
    1 {- A -} ^ {- B -} 2 {- C -} % {- D -} 3 {- E -} ^ {- F -} 4


looserBinding =
    1 + 2 % 3 + 4


looserBinding_comments =
    1 {- A -} + {- B -} 2 {- C -} % {- D -} 3 {- E -} + {- F -} 4


fullyApplied =
    (%) -1 4


fullyApplied_comments =
    (%) {- A -} -1 {- B -} 4


partiallyApplied =
    (%) 1


unapplied =
    (%)


extraArgs =
    (%) 1 2 3 4
