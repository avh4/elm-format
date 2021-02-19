module Declarations exposing (MyTypeReference, MyTypeReferenceWithArguments, MyTypeVariable, MyUnit)


type alias MyUnit =
    ()


type alias MyTypeReference =
    Basics.String


type alias MyTypeReferenceWithArguments =
    Result.Result () ()


type alias MyTypeVariable a =
    a
