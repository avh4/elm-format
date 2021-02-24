module Declarations exposing (MyCustomType(..), MyTypeAlias, MyTypeAliasWithParameters)


type alias MyTypeAlias =
    ()


type alias MyTypeAliasWithParameters x y =
    ()


type MyCustomType a b
    = Both a b
    | None
