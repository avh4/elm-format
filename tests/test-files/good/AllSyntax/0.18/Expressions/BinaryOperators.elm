module Main exposing (..)


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
