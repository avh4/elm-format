module Declarations exposing (MyCustomType(..), MyTypeAlias, MyTypeAliasWithParameters, noTypeAnnotation, withParametersAndTypeAnnotation, withTypeAnnotation)


type alias MyTypeAlias =
    ()


type alias MyTypeAliasWithParameters x y =
    ()


type MyCustomType a b
    = Both a b
    | None


noTypeAnnotation =
    ()


withTypeAnnotation : ()
withTypeAnnotation =
    ()


withParametersAndTypeAnnotation : Int -> String -> ()
withParametersAndTypeAnnotation a b =
    ()
