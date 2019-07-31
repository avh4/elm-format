module Main exposing (Foo(..), X, check, x)


x =
    ()


type alias X =
    String


type Foo
    = A
    | B
    | C


port check : String -> Cmd msg
