module Patterns exposing (closedList, data, emptyList, emptyRecord, literals, openList, record, tuple, unit, variables, wildcard)


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


emptyList [] =
    ()


closedList [ _, _ ] =
    ()


openList (_ :: _ :: _) =
    ()


emptyRecord {} =
    ()


record { x, y } =
    ()
