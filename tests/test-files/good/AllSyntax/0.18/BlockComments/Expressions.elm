module AllSyntax.BlockComments.Expressions exposing (..)


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
