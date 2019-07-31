module Main exposing (infixApplication, infixApplicationMultiline)


infixApplication =
    1 `operator` 2


infixApplicationMultiline =
    x
        `infixOperator`
            [ y
            , z
            ]
