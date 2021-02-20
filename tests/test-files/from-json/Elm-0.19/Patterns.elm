module Patterns exposing (data, literals, tuple, unit, variables, wildcard)


wildcard _ =
    ()


literals 1 2.0 "string" 'c' =
    ()


variables x =
    ()


data (Foo _ _) =
    ()


unit () =
    ()


tuple ( _, _ ) =
    ()
