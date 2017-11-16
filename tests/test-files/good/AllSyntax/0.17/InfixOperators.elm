module Main exposing (..)


infixApplication =
    1 `operator` 2


infixApplicationMultiline =
    x
        `infixOperator`
            [ y
            , z
            ]
