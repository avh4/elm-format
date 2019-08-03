module ConvertBasicsModBy exposing (binop)


binop =
    modBy 4 -1


binop_comments =
    modBy {- B -} 4 {- A -} -1


tigherBinding =
    modBy (3 ^ 4) (1 ^ 2)


tigherBinding_comments =
    modBy {- D -} (3 {- E -} ^ {- F -} 4) {- C -} (1 {- A -} ^ {- B -} 2)


looserBinding =
    1 + modBy 3 2 + 4


looserBinding_comments =
    1 {- A -} + {- B -} modBy {- D -} 3 {- C -} 2 {- E -} + {- F -} 4


fullyApplied =
    modBy 4 -1


fullyApplied_comments =
    modBy {- B -} 4 {- A -} -1


partiallyApplied =
    \modulus -> modBy modulus 1


unapplied =
    \dividend modulus -> modBy modulus dividend


extraArgs =
    modBy 2 1 3 4
