module Main exposing (commentedInfixOperator, infixOperator, leftPipe, lineComments, multilineInfixOperators)


infixOperator =
    1 + 2 * 3 / 4 <> 5 |> (+) 0


multilineInfixOperators =
    1
        + 2
        * 3
        / 4
        <> 5
        |> (+) 0


commentedInfixOperator =
    1 {- plus -} + 2


lineComments =
    ()
        |> identity
        |> identity
        --X
        |> identity


leftPipe a =
    a <|
        a <|
            a <|
                ()
