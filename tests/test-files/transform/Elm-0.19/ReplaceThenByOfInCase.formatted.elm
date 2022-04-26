module Main exposing (Foo, a, b)


type Foo
    = A
    | B
    | C


a =
    A


b =
    case a of
        A ->
            1

        B ->
            2

        C ->
            3

