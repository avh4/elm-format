module Declarations exposing (MyFunctionType, MyTupleType, MyTypeReference, MyTypeReferenceWithArguments, MyTypeVariable, MyUnit)


type alias MyUnit =
    ()


type alias MyTypeReference =
    Basics.String


type alias MyTypeReferenceWithArguments =
    Result.Result () ()


type alias MyTypeVariable a =
    a


type alias MyTupleType =
    ( (), () )


type alias MyFunctionType =
    () -> () -> ()
