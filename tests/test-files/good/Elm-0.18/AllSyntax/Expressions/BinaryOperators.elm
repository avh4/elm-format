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


{-| elm-format will auto-correct `=>` to `->` when used in types and lambda introductions,
but for Elm <= 0.18, it is valid as an expression and should be untouched in that usage.
-}
fatArrowOperator =
    let
        (=>) a b =
            ()
    in
    1 => 2 => 3
