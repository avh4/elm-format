module AllSyntax.BlockComments.Expressions exposing (..)


unit =
    ({- A -})


emptyList =
    [{- B -}]


ifStatement =
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


caseStatement =
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
