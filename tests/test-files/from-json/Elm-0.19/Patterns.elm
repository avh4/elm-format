module Patterns exposing (data, literals, variables, wildcard)


wildcard _ =
    ()


literals 1 2.0 "string" 'c' =
    ()


variables x =
    ()


data (Foo _ _) =
    ()
